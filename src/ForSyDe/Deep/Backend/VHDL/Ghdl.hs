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
module ForSyDe.Deep.Backend.VHDL.Ghdl (compileResultsGhdl,
                                       executeTestBenchGhdl) where

import ForSyDe.Deep.Backend.VHDL.Traverse.VHDLM
import ForSyDe.Deep.Backend.VHDL.TestBench

import ForSyDe.Deep.System.SysDef
import ForSyDe.Deep.OSharing
import ForSyDe.Deep.ForSyDeErr
import ForSyDe.Deep.Config (getDataDir)

import Data.List (intersperse)
import Data.Maybe (isJust)
import Control.Monad (liftM, when)
import Control.Monad.State (gets)
import System.Directory (findExecutable,
                         setCurrentDirectory,
                         getTemporaryDirectory,
                         createDirectoryIfMissing)
import System.Process (runProcess, waitForProcess)
import System.Exit (ExitCode(..))
import System.IO 
import System.FilePath ((</>))
import qualified Language.Haskell.TH as TH (Exp)

data GhdlCommand = Analyze | Elaborate | Compile | Import | Run deriving Eq
instance Show GhdlCommand where
    show Analyze   = "-a"
    show Elaborate = "-e"
    show Compile   = "-c"
    show Import    = "-i"
    show Run       = "-r"

type Path = String

data GhdlEnv =  GhdlEnv { sysId           :: String
                        , sysTb           :: String
                        , syslib          :: String
                        , tbFile          :: Path
                        , tbExecutable    :: Path
                        , libFile         :: Path
                        , workFiles       :: [Path]
                        , forsydeLibFile  :: Path
                        , forsydeLibDir   :: Path
                        , systemLibDir    :: Path
                        , workDir         :: Path
                        , paths           :: [Path]
                        }

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
 runGhdlCommand Analyze "forsyde" 
                        (forsydeLibDir env) 
                        [] 
                        [forsydeLibFile env] 
                        []

 -- analyze the generated system library
 runGhdlCommand Analyze (syslib env) 
                        (systemLibDir env) 
                        [forsydeLibDir env] 
                        [libFile env] 
                        []

 -- compile test bench hierarchy
 runGhdlCompile (sysTb env)
                (workDir env) 
                [forsydeLibDir env, systemLibDir env] 
                tbFile env:workFiles env

 -- TODO: run test bench with "tb_out.txt" as output file
 runGhdlCommand Run (sysTb env) 
                    (workDir env) 
                    [forsydeLibDir, systemLibDir] 
                    [] 
                    ["--stop-time="++show (cycles*10)++"ns"]

 -- read test output data
 testOutput <- liftIO $ do 
         handle <- openFile file ReadMode
         output <- hGetContents handle
         -- go back to the original directory
         setCurrentDirectory (".." </> "..")
         return output

 parseTestBenchOut testOutput


runGhdlCompile :: String -> Path -> [Path] -> [Path] -> VHDLM ()
runGhdlCompile toplevel workdir libPaths files = 
  runGhdlCommand Compile "work" workdir libPaths files extra
    where
      extra = [show Elaborate,
               toplevel]

runGhdlCommand :: GhdlCommand 
                -> String -> Path -> [Path] -> [Path] 
                -> [String] 
                -> VHDLM ()
runGhdlCommand command libName workdir libPaths files extraOpts =
  runCommand "ghdl" $ cmd ++ paths ++ opts ++ files ++ extraOpts
    where
      cmd   = [show command]
      paths = map ("-P"++) libPaths
      opts  = ["--work="++libName, 
               "--workdir="++workdir]


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

