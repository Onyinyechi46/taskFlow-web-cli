module Todo.IO
  ( createTodoFile
  , addTaskFile
  , listTaskFile
  , removeTaskFile
  ) where

import System.IO (writeFile, appendFile, readFile)
import System.Directory (doesFileExist)
import Prelude

-- | Create an empty to-do file
createTodoFile :: [String] -> IO ()
createTodoFile (file:_) = do
    writeFile file ""
    putStrLn $ "✅ Created new to-do list: " ++ file
createTodoFile _ = putStrLn "⚠️  Please specify a file name."

-- | Add tasks to file
addTaskFile :: [String] -> IO ()
addTaskFile (file:tasks) =
    if null tasks
    then putStrLn "⚠️  No tasks provided to add."
    else do
        appendFile file $ unlines tasks
        putStrLn $ "✅ Added " ++ show (length tasks) ++ " task(s)."
addTaskFile _ = putStrLn "⚠️  Usage: todo <file> add <task1> <task2> ..."

-- | List tasks from file
listTaskFile :: [String] -> IO ()
listTaskFile (file:_) = do
    exists <- doesFileExist file
    if not exists
    then putStrLn $ "🚫 File not found: " ++ file
    else do
        contents <- readFile file
        let tasks = lines contents
        if null tasks
        then putStrLn "📭 No tasks found."
        else mapM_ (\(i, t) -> putStrLn $ show i ++ ". " ++ t) (zip [1..] tasks)
listTaskFile _ = putStrLn "⚠️  Usage: todo <file> list"

-- | Remove task by index
removeTaskFile :: [String] -> IO ()
removeTaskFile (file:nStr:_) = do
    exists <- doesFileExist file
    if not exists
    then putStrLn $ "🚫 File not found: " ++ file
    else case reads nStr of
        [(n, "")] -> do
            contents <- readFile file
            let tasks = lines contents
            if n < 1 || n > length tasks
            then putStrLn "❌ Invalid task number."
            else do
                let updated = unlines $ take (n - 1) tasks ++ drop n tasks
                writeFile file updated
                putStrLn $ "🗑️  Deleted task #" ++ show n
        _ -> putStrLn "⚠️  Please enter a valid task number."
removeTaskFile _ = putStrLn "⚠️  Usage: todo <file> delete <task-number>"
