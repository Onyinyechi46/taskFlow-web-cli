import System.Environment (getArgs)
import Todo.IO (createTodoFile, addTaskFile, listTaskFile, removeTaskFile)

main :: IO ()
main = do
    args <- getArgs
    processArgs args

processArgs :: [String] -> IO ()
processArgs (file:"new":_)    = createTodoFile [file]
processArgs (file:"add":rest) = addTaskFile (file : rest)
processArgs (file:"list":_)   = listTaskFile [file]
processArgs (file:"delete":n:_) = removeTaskFile [file, n]
processArgs _ = putStrLn "❓ Usage: <file> new|add|list|delete"
