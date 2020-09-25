import System.Directory (renameFile, removeFile)
import System.Exit (ExitCode(..))
import System.Process (createProcess, waitForProcess, shell)
import System.Environment (getArgs)
import System.IO (hPutStrLn, stderr)

main :: IO ()
main = do
  args <- getArgs
  mapM_ redo args

redo :: String -> IO ()
redo target = do
  let tmp = target ++ "---redoing"
  (_, _, _, prHandle) <- createProcess $ shell $ "bash " ++ target ++ ".do - - " ++ tmp ++ " > " ++ tmp
  exit <- waitForProcess prHandle 
  case exit of 
    ExitSuccess -> do renameFile tmp target
    ExitFailure code -> do hPutStrLn stderr $ "Redo script exited w/ non-zero exit code: " ++ show code 
                           removeFile tmp
  
