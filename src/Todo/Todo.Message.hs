module Todo.Messages where

todoFileNotExistsMessage :: String -> String
todoFileNotExistsMessage file = "File does not exist: \"" ++ file ++ "\"."

notSpecifiedMessage :: String -> String
notSpecifiedMessage arg = "No " ++ arg ++ " specified."

noTaskFileSpecified :: String
noTaskFileSpecified = notSpecifiedMessage "task file name"

noTaskIDSpecifiedMessage :: String
noTaskIDSpecifiedMessage = notSpecifiedMessage "task ID"

noTaskSpecifiedMessage :: String
noTaskSpecifiedMessage = notSpecifiedMessage "task"

fileAlreadyExistsMessage :: String -> String
fileAlreadyExistsMessage file = "File already exists: \"" ++ file ++ "\"."

todoFileCreatedMessage :: String -> String
todoFileCreatedMessage file = "Todo list file created: \"" ++ file ++ "\"."

noTasksToDeleteMessage :: String
noTasksToDeleteMessage = "There are currently no tasks to delete."

noTaskWithIDMessage :: String -> String -> String
noTaskWithIDMessage tid file = "There is no task with ID \"" ++ tid ++ "\" in file \"" ++ file ++ "\"."
