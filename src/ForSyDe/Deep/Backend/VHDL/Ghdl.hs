-----------------------------------------------------------------------------
-- |
-- Module      :  ForSyDe.Deep.Backend.VHDL.Modelsim
-- Copyright   :  (c) ES Group, KTH/ICT/ES 2007-2013
-- License     :  BSD-style (see the file LICENSE)
-- 
-- Maintainer  :  forsyde-dev@ict.kth.se
-- Stability   :  experimental
-- Portability :  portable
--
-- Functions to process the VHDL compilation results with Modelsim.
-----------------------------------------------------------------------------
module ForSyDe.Deep.Backend.VHDL.Modelsim (compileResultsGhdl,
                                      executeTestBenchGhdl) where

import ForSyDe.Deep.Backend.VHDL.Traverse.VHDLM
import ForSyDe.Deep.Backend.VHDL.TestBench

import ForSyDe.Deep.System.SysDef
import ForSyDe.Deep.OSharing
import ForSyDe.Deep.ForSyDeErr
import ForSyDe.Deep.Config (getDataDir)

import Data.List (intersperse)
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

-- | Generate a testbench and execute it with Modelsim
--   (Note: the initial and final CWD will be / )
executeTestBenchGhdl :: Maybe Int -- ^ Number of cycles to simulate          
                         -> [[TH.Exp]] -- ^ input stimuli, each signal value
                                       --   is expressed as a template haskell
                                       --   expression 
                         -> VHDLM [[String]] -- ^ results, each signal value
                                             --   is expressed as a string
executeTestBenchGhdl mCycles stimuli = do
 -- Check if modelsim is installed
 installed <- liftIO $ isGhdlInstalled
 when (not installed) (throwFError ModelsimFailed) 
 
 -- compile testbench
 cycles <- writeVHDLTestBench mCycles stimuli
 sysid <- gets (sid.globalSysDef.global)
 -- change to sysid/vhdl/
 liftIO $ setCurrentDirectory (sysid </> "vhdl")

 -- get the name of the vhdl files to compile
 sys <- gets (globalSysDef.global)
 dataPath <- liftIO $ getDataDir
 let sysId     = sid sys
     sysTb     = sysId ++ "_tb"
     syslib    = sysId ++ "_lib"
     libFile   = syslib </> (syslib ++ ".vhd")
     tbFile    = "test" </> (sysTb ++ ".vhd")
     workFiles = ("work" </> (sysId ++ ".vhd")) :
          map (("work"</>).(++".vhd").sid.readURef.unPrimSysDef)
              (subSys sys)
     forsydeLibDir = "forsyde"</>"ghdl"
     systemLibDir  =  syslib  </>"ghdl"
     workDir       = "work"   </>"ghdl"
     paths = [forsydeLibDir, systemLibDir, workDir]
 tmpdir <- liftIO getTemporaryDirectory 
 (file, handle) <- liftIO $ openTempFile tmpdir "tb_out.txt"
 -- we close the temporal file to avoid opening problems with vsim on windows
 liftIO $ hClose handle 

 liftIO $ mapM (createDirectoryIfMissing True) paths

 runGhdlCommand Analyze "forsyde" forsydeLibDir [] [dataPath</>"forsyde.vhd"] []
 runGhdlCommand Analyze syslib systemLibDir [forsydeLibDir] [libFile] []
 runGhdlCompile sysTb workDir [forsydeLibDir, systemLibDir] workFiles

 handle2 <- liftIO $ openFile file ReadMode
 flatOut <- liftIO $ hGetContents handle2

 -- go back to the original directory
 liftIO $ setCurrentDirectory (".." </> "..")
 parseTestBenchOut flatOut

-- | Compile the generated VHDL code with GHDL
--   (Note: the initial and final CWD will be /systemName/vhdl )
compileResultsGhdl :: VHDLM ()
compileResultsGhdl = do
 -- Check if modelsim is installed
 installed <- liftIO $ isGhdlInstalled
 when (not installed) (throwFError ModelsimFailed) 
 -- get the name of the vhdl files to compile
 sys <- gets (globalSysDef.global)
 dataPath <- liftIO $ getDataDir
 let sysId = sid sys
     syslib = sysId ++ "_lib"
     libFile = syslib </> (syslib ++ ".vhd")
     workFiles = ("work" </> (sysId ++ ".vhd")) :
          map (("work"</>).(++".vhd").sid.readURef.unPrimSysDef)
              (subSys sys)
     forsydeLibDir = "forsyde"</>"ghdl"
     systemLibDir  =  syslib  </>"ghdl"
     workDir       = "work"   </>"ghdl"
     paths = [forsydeLibDir, systemLibDir, workDir]

 liftIO $ mapM (createDirectoryIfMissing True) paths

 runGhdlCommand Analyze "forsyde" forsydeLibDir [] [dataPath</>"forsyde.vhd"] []
 runGhdlCommand Analyze syslib systemLibDir [forsydeLibDir] [libFile] []
 runGhdlCommand Analyze "work" workDir [forsydeLibDir, systemLibDir] workFiles []



data GhdlCommand = Analyze | Elaborate | Compile | Import deriving Eq
instance Show GhdlCommand where
    show Analyze   = "-a"
    show Elaborate = "-e"
    show Compile   = "-c"
    show Import    = "-i"
type Path = String

runGhdlCompile :: String -> Path -> [Path] -> [Path] -> VHDLM ()
runGhdlCompile toplevel workdir libPaths files
  runGhdlCommand Compile "work" workdir libPaths files extra
    where
      extra = [show Elaborate,
               toplevel]

runGhdlCommand :: GhdlCommand 
                -> String -> Path -> [Path] -> [Path] 
                -> [String] 
                -> VHDLM ()
runGhdlCommand command libName workdir libPaths files extraOpts = do
  runCommand "ghdl" $ cmd ++ paths ++ opts ++ files ++ extraOpts
    where
      cmd   = [show command]
      paths = map ("-P"++) libPaths
      opts  = ["--work="++libName, 
               "--workdir="++workdir]


-- | run a ModelSim command
runCommand :: String -- ^ Command to execute 
              -> [String] -- ^ Command arguments
              -> VHDLM ()
runCommand command args = do
  success <- liftIO $ runWait msg command args
  when (not success) (throwFError ModelsimFailed)
 where msg = "Running: " ++ command ++ " " ++ (concat $ intersperse " " args)


-- | Run a process, previously announcing a message and waiting for it
--   to finnish its execution.
runWait :: String -- ^ message to show
        -> FilePath -- ^ command to execute 
        -> [String] -- ^ command arguments
        -> IO Bool -- ^ Did the execution end succesfully?
runWait msg proc args = do
           putStrLn msg 
           h <- runProcess proc args Nothing Nothing Nothing Nothing Nothing
           code <- waitForProcess h
           return $ code == ExitSuccess 


-- Look for modelsim executables
isGhdlInstalled :: IO Bool
isGhdlInstalled =  executablePresent "ghdl"
 where executablePresent = (liftM (maybe False (\_-> True))) .findExecutable

