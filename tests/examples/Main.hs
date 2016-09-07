-- Property-testing wrapper module
module Main (main) where
import Test.HUnit
import System.Exit
import System.IO
import System.Directory (createDirectoryIfMissing, setCurrentDirectory)
import VHDLBackend (vhdlBackendTest)


main :: IO ()
main = do
    hSetBuffering stdout LineBuffering
    putStrLn "Running ForSyDe's unit test suite"
    createDirectoryIfMissing True workdir
    setCurrentDirectory workdir
    runTestCount $ test ["VHDL Backend Test" ~: vhdlBackendTest ]
  where runTestCount t = do (c, _) <- myRunTestText t 
                            if errors c /= 0 || failures c /= 0 
                               then exitFailure 
                               else exitWith ExitSuccess
        myRunTestText = runTestText (PutText (\str _ _ -> putStrLn str) ())
        workdir = "dist/test/temp"
