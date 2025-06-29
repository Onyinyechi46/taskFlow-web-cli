{-# LANGUAGE OverloadedStrings #-}

module Main where

import Web.Scotty
import Network.Wai.Middleware.Static
import Data.Text.Lazy (pack)
import Control.Monad.IO.Class (liftIO)
import Todo.IO (addTaskFile, removeTaskFile)
import System.IO (readFile)

main :: IO ()
main = scotty 3000 $ do
  -- Serve everything in /public as static files
  middleware $ staticPolicy (addBase "public")

  -- Serve index.html
  get "/" $ file "public/index.html"

  -- Add task
  post "/add-task" $ do
    task <- formParam "task"
    due  <- formParam "due"
    liftIO $ addTaskFile ["tasks.txt", task ++ " (Due: " ++ due ++ ")"]
    text "✅ Task added!"

  -- Show tasks
  get "/tasks" $ do
    tasks <- liftIO $ readFile "tasks.txt"
    text (pack tasks)

  -- Delete task
  post "/delete-task" $ do
    line <- formParam "line"
    liftIO $ removeTaskFile ["tasks.txt", line]
    text "🗑️ Task deleted!"
