import System.Process

main = do
  _ <- createProcess $ shell "bash redo.do"
  return () 
