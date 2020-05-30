module Todo.IO where

import Prelude   hiding (readFile)
import System.IO.Strict (readFile)
import System.Directory (doesFileExist)

import Todo.List
import Todo.Messages

createTodoFile :: [String] -> IO ()
createTodoFile [] = putStrLn noTaskFileSpecified
createTodoFile (file:[]) = do
    exists <- doesFileExist file
    if not exists then do
        writeFile file ""
        putStrLn (todoFileCreatedMessage file)
    else
        putStrLn (fileAlreadyExistsMessage file)

addTaskFile :: [String] -> IO ()
addTaskFile []           = putStrLn noTaskFileSpecified
addTaskFile (_:[])       = putStrLn noTaskSpecifiedMessage
addTaskFile (file:tasks) = do
    todo <- readFile file
    let newTodo = foldl (addTask) (lines todo) tasks
    writeFile file (unlines newTodo)

removeTaskFile :: [String] -> IO ()
removeTaskFile []     = putStrLn noTaskFileSpecified
removeTaskFile (_:[]) = putStrLn noTaskIDSpecifiedMessage
removeTaskFile (file:tid:[]) = do
    todo <- readFile file
    let tasks = lines todo
    if (length tasks) >= (read tid) then
        writeFile file (unlines (removeTask tasks (read tid)))
    else if (length tasks) == 0 then
        putStrLn noTasksToDeleteMessage
    else
        putStrLn (noTaskWithIDMessage file tid)

listTaskFile :: [String] -> IO ()
listTaskFile (file:[]) = do
    todo <- readFile file
    let tasks = lines todo
    putStr . unlines . listTask $ tasks