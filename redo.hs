import Control.Monad (filterM)
import System.Directory (doesFileExist, renameFile, removeFile)
import System.Exit (ExitCode(..))
import System.FilePath (hasExtension, replaceBaseName)
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
  maybePath <- redoPath target
  case maybePath of
    Nothing -> error $ "No .do files found for target '" ++ target ++ "'"
    Just path -> do
      (_, _, _, prHandle) <- createProcess $ shell $ "bash " ++ path ++ " - - " ++ tmp ++ " > " ++ tmp
      exit <- waitForProcess prHandle 
      case exit of 
        ExitSuccess -> do renameFile tmp target
        ExitFailure code -> do hPutStrLn stderr $ "Redo script exited w/ non-zero exit code: " ++ show code 
                               removeFile tmp
  

redoPath :: FilePath -> IO (Maybe FilePath)
redoPath target = do
  realCandidates <- filterM doesFileExist candidates
  (.) return  safeHead realCandidates 
    where candidates = [target ++ ".do"] ++ if hasExtension target then [replaceBaseName target "default" ++ ".do"] else []
          safeHead [] = Nothing
          safeHead (x:xs) = Just x
