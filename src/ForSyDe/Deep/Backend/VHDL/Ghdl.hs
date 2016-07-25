-----------------------------------------------------------------------------
-- |
-- Module      :  ForSyDe.Deep.Backend.VHDL.Ghdl
-- Copyright   :  (c) ES Group, KTH/ICT/ES 2007-2013
-- License     :  BSD-style (see the file LICENSE)
-- 
-- Maintainer  :  forsyde-dev@ict.kth.se
-- Stability   :  experimental
-- Portability :  portable
--
-- Functions to process the VHDL compilation results with GHDL.
-----------------------------------------------------------------------------
module ForSyDe.Deep.Backend.VHDL.Ghdl (executeTestBenchGhdl) where

import ForSyDe.Deep.Backend.VHDL.Traverse.VHDLM
import ForSyDe.Deep.Backend.VHDL.TestBench

import ForSyDe.Deep.System.SysDef
import ForSyDe.Deep.OSharing
import ForSyDe.Deep.ForSyDeErr
import ForSyDe.Deep.Config (getDataDir)

import Data.Maybe (isJust)
import Control.Monad.State (gets)
import System.Directory (findExecutable,
                         setCurrentDirectory,
                         getTemporaryDirectory,
                         createDirectoryIfMissing)
import System.Process (readProcessWithExitCode, readProcess, runProcess, waitForProcess)
import System.Exit (ExitCode(..))
import System.IO 
import System.FilePath ((</>))
import qualified Language.Haskell.TH as TH (Exp)

--
-- This tool driver needs a Ghdl version containing the following commit,
-- otherwise it will fail with an obscure pattern match failure from within TH
-- generated code
--
-- commit f6d8e786a1ca3165b41cea7de05b8f2151ac31ff
-- Author: Tristan Gingold <tgingold@free.fr>
-- Date:   Sat May 30 14:05:20 2015 +0200
--
--     write: do not implicitely append LF.
--
-- The oldest release containing this was v0.33 
--
data GhdlCommand = Analyze | Elaborate | Compile | Import | Run deriving Eq

instance Show GhdlCommand where
    show Analyze   = "-a"
    show Elaborate = "-e"
    show Compile   = "-c"
    show Import    = "-i"
    show Run       = "-r"

data GhdlEnv =  GhdlEnv { sysId           :: String
                        , sysTb           :: String
                        , syslib          :: String
                        , tbFile          :: FilePath
                        , tbExecutable    :: FilePath
                        , libFile         :: FilePath
                        , workFiles       :: [FilePath]
                        , forsydeLibFile  :: FilePath
                        , forsydeLibDir   :: FilePath
                        , systemLibDir    :: FilePath
                        , workDir         :: FilePath
                        , paths           :: [FilePath]
                        }

mkGhdlEnv :: SysDefVal -> FilePath -> GhdlEnv
mkGhdlEnv sys osDataPath = 
    GhdlEnv
      { sysId     = sysId
      , sysTb     = sysTb
      , syslib    = syslib
      , libFile   = syslib </> (syslib ++ ".vhd")
      , tbFile    = "test" </> (sysTb ++ ".vhd")
      , forsydeLibFile = osDataPath</>"lib"</>"forsyde.vhd"
      , workFiles = ("work" </> (sysId ++ ".vhd")) :
             map (("work"</>).(++".vhd").sid.readURef.unPrimSysDef)
                 (subSys sys)
      , forsydeLibDir = forsydeLibDir
      , systemLibDir  = systemLibDir
      , workDir       = workDir
      , tbExecutable  = workDir  </> sysTb
      , paths = [forsydeLibDir, systemLibDir, workDir]
    }
    where
        sysId           = sid sys
        syslib          = sysId ++ "_lib"
        sysTb           = sysId ++ "_tb"
        workDir         = "work"    </> "ghdl"
        forsydeLibDir   = "forsyde" </> "ghdl"
        systemLibDir    = syslib    </> "ghdl"


-- | Generate a testbench and execute it with GHDL
--   (Note: the initial and final CWD will be / )
executeTestBenchGhdl :: Maybe Int -- ^ Number of cycles to simulate          
                         -> [[TH.Exp]] -- ^ input stimuli, each signal value
                                       --   is expressed as a template haskell
                                       --   expression 
                         -> VHDLM [[String]] -- ^ results, each signal value
                                             --   is expressed as a string
executeTestBenchGhdl mCycles stimuli = do
 -- Check if GHDL is installed
 installed <- liftIO isGhdlInstalled
 unless installed (throwFError GhdlFailed) 
 
 -- compile testbench
 cycles <- writeVHDLTestBench mCycles stimuli

 -- assemble all the file paths for compilation
 sysid    <- gets (sid.globalSysDef.global)
 sys      <- gets (globalSysDef.global)
 dataPath <- liftIO getDataDir
 let env = mkGhdlEnv sys dataPath

 -- set up directory structure and tmp files
 file <- liftIO $ do
        setCurrentDirectory (sysid </> "vhdl")
        tmpdir         <- getTemporaryDirectory 
        (file, handle) <- openTempFile tmpdir "tb_out.txt"
        hClose handle -- close handle to avoid opening problems in windows
        mapM_ (createDirectoryIfMissing True) $ paths env
        return file

 -- analyze the installed forsyde library
 runGhdlCommand Analyze "forsyde"               -- library (toplevel)
                        (forsydeLibDir env)     -- workdir
                        []                      -- include paths
                        [forsydeLibFile env]    -- files

 -- analyze the generated system library
 runGhdlCommand Analyze (syslib env)            -- library (toplevel)
                        (systemLibDir env)      -- workdir
                        [forsydeLibDir env]     -- include paths
                        [libFile env]           -- files

 -- compile test bench hierarchy
 runGhdlCompile (sysTb env)                             -- toplevel
                (workDir env)                           -- workdir
                [forsydeLibDir env, systemLibDir env]   -- include paths
                (tbFile env:workFiles env)              -- files

 -- Run simulation and capture output
 testOutput <- runGhdlSim (sysTb env) cycles

 liftIO $ setCurrentDirectory (".." </> "..")
 --liftIO $ print testOutput -- show what we actually get from Ghdl

 parseTestBenchOut testOutput

runGhdlCompile :: String -> FilePath -> [FilePath] -> [FilePath] -> VHDLM ()
runGhdlCompile toplevel workdir libPaths files = 
  runGhdlCommandInWorkdir Compile "work" workdir libPaths files extra
    where
      extra = [show Elaborate,
               toplevel]


runGhdlCommand :: GhdlCommand 
                -> String -> FilePath -> [FilePath] -> [FilePath] 
                -> VHDLM ()
runGhdlCommand cmd lib work paths files = 
        runGhdlCommandInWorkdir cmd lib work paths files []

runGhdlCommandInWorkdir :: GhdlCommand 
                -> String -> FilePath -> [FilePath] -> [FilePath] 
                -> [String] 
                -> VHDLM ()
runGhdlCommandInWorkdir command libName workdir libPaths files extraOpts =
  runCommand "ghdl" $ cmd ++ paths ++ opts ++ files ++ extraOpts
    where
      cmd   = [show command]
      paths = map ("-P"++) libPaths
      opts  = ["--work="++libName, 
               "--workdir="++workdir]

runGhdlSim :: String -> Int -> VHDLM String
runGhdlSim toplevel cycles = do
    (output,success) <- liftIO $ do 
          putStrLn $ "Running: ghdl " ++ unwords args
          (exitcode,stdout,stderr) <- readProcessWithExitCode "ghdl" args stdin
          let success = exitcode == ExitSuccess
          return (stdout,success)
    unless success (throwFError GhdlFailed)
    return output
  where
    stdin = ""
    args = [show Run, toplevel, "--stop-time="++show (cycles*10)++"ns"]


-- | run a shell command
runCommand :: String -- ^ Command to execute 
              -> [String] -- ^ Command arguments
              -> VHDLM ()
runCommand command args = do
  success <- liftIO $ do
      putStrLn msg 
      h <- runProcess command args Nothing Nothing Nothing Nothing Nothing
      code <- waitForProcess h
      return $ code == ExitSuccess 
  unless success (throwFError GhdlFailed)
 where msg = "Running: " ++ command ++ " " ++ unwords args


-- Look for GHDL executables
isGhdlInstalled :: IO Bool
isGhdlInstalled =  executablePresent "ghdl"
 where executablePresent = liftM isJust .findExecutable

