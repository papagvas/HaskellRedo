import Control.Monad (filterM)
import Data.Map.Lazy (fromList, toList, adjust, insert)
import System.Directory (doesFileExist, renameFile, removeFile)
import System.Exit (ExitCode(..))
import System.FilePath (hasExtension, replaceBaseName, takeBaseName)
import System.Process (createProcess, waitForProcess, shell, CreateProcess(..))
import System.Environment (getArgs, getEnvironment)
import System.IO (hPutStrLn, stderr)

main :: IO ()
main = getArgs >>= mapM_ redo
  
  

redo :: String -> IO ()
redo target = do
  let tmp = target ++ "---redoing"
  maybePath <- redoPath target
  oldEnv <- getEnvironment
  let newEnv = toList $ adjust (++ ":.") "PATH" $ insert "REDO_TARGET" target $ fromList oldEnv
  case maybePath of
    Nothing -> error $ "No .do files found for target '" ++ target ++ "'"
    Just path -> do
      (_, _, _, prHandle) <- createProcess $ (shell $ unwords ["sh", path, ".do", "0", takeBaseName target, tmp, ">", tmp]) { env = Just newEnv } 
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
