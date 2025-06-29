module Todo.List where

type FormattedList = [String]
type TodoList      = [String]
type Task          = String
type TaskID        = Int

-- | Add a new task to the to-do list.
addTask :: TodoList -> Task -> TodoList
addTask tasks new = tasks ++ [new]

-- | Remove a task by ID (1-based). Safe and won't crash if ID is out of bounds.
removeTask :: TodoList -> TaskID -> TodoList
removeTask tasks tid
    | tid < 1 || tid > length tasks = tasks  -- invalid ID, return original
    | otherwise = take (tid - 1) tasks ++ drop tid tasks

-- | Format the list of tasks with numbering.
listTask :: TodoList -> FormattedList
listTask tasks = zipWith formatTask [1..] tasks
  where
    formatTask n task = show n ++ " - " ++ task
