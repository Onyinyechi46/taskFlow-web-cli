module Main (main) where

import System.Environment
import System.Directory (doesFileExist)

import Todo.IO
import Todo.Messages(todoFileNotExistsMessage)

main :: IO ()
main = do
    -- Retrieve file, command, and arguments.
    commandArgs <- getArgs
    processArgs commandArgs

processArgs :: [String] -> IO ()
processArgs []                  = do putStrLn helpMessage
processArgs (file:[])           = do putStrLn helpMessage
processArgs (file:command:args) = do
    -- Determine whether corresponding todo list file exists.
    exists <- doesFileExist file

    if command == "create" || exists then do
        -- If the user supplied command exists, perform the command.
        case lookup command dispatch of
            Just action -> action (file:args)
            Nothing     -> putStrLn (invalidCommandMessage command)
    else
        -- Notify user if they're trying to interact with a non-existent todo list.
        putStrLn (todoFileNotExistsMessage file)

dispatch :: [(String, [String] -> IO ())]
dispatch = map (extractActions) options
    where extractActions = \(command, _, action) -> (command, action)

helpMessage :: String
helpMessage = unlines ((map (formatUsage) usage) ++ (map (formatOption) options))
    where formatOption = \(command, desc, _) -> " - " ++ command ++ ": \t" ++ desc
          formatUsage  = \(command, desc)    -> command ++ ": " ++ desc ++ "\n"

usage :: [(String, String)]
usage = [ ("Usage",   "todo FILE OPTION [ARGUMENT]...")
        , ("Options", "")
        ]

options :: [(String, String, [String] -> IO ())]
options = [ ("list",   "List tasks in todo list.", listTaskFile)
          , ("create", "Create a new todo list.", createTodoFile)
          , ("add",    "Add a new task to the specified todo list.", addTaskFile)
          , ("delete", "Delete task with the specified ID from the todo list.", removeTaskFile)
          ]

invalidCommandMessage :: String -> String
invalidCommandMessage command = "Invalid command: '" ++ command ++ "'."

