module Todo.List where

import Data.List (delete)

type FormattedList = [String]
type TodoList      = [String]
type Task          = String
type TaskID        = Int

addTask :: TodoList -> Task -> TodoList
addTask tasks new = tasks ++ [new]

removeTask :: TodoList -> TaskID -> TodoList
removeTask tasks tid = delete (tasks !! (tid - 1)) tasks

listTask :: TodoList -> FormattedList
listTask tasks = zipWith (formatTask) [1..] tasks
    where formatTask = \n task -> show n ++ " - " ++ task