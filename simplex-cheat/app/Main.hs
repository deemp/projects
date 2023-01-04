{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PostfixOperators #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Main (main) where

import Control.Concurrent
import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.Monad
import Control.Monad.IO.Class
import Data.Aeson
import Data.Text (Text)
import GHC.Generics
import Network.Wai.Handler.Warp
import Servant
import Servant.Docs (API, DocCapture (DocCapture), ToCapture (toCapture), ToSample (toSamples), docs, markdown, singleSample)
import StmContainers.Map as Map (Map, insert, lookup, newIO)
import qualified System.IO as IO
import System.Random.Stateful (globalStdGen, uniformRM)
import Prelude hiding (lookup)

data Status
  = Finished {result :: Int}
  | InProcess
  deriving stock (Generic)
  deriving anyclass (FromJSON, ToJSON)

type StartTask =
  "task"
    :> "start"
    :> ReqBody '[JSON] Int
    :> Post '[JSON] ID

-- curl -X POST -d '3' -H 'Content-Type: application/json' loalhost:8081/task/start

type CheckTask =
  "task"
    :> "status"
    :> Capture "id" ID
    :> Get '[JSON] Status

-- curl -X GET localhost:8081/task/status/1

instance ToCapture (Capture "id" ID) where
  toCapture :: Proxy (Capture "id" ID) -> DocCapture
  toCapture _ = DocCapture "id" "(integer) task data"

instance ToSample Status where
  toSamples :: Proxy Status -> [(Text, Status)]
  toSamples _ = singleSample (Finished 6)

instance ToSample Int where
  toSamples :: Proxy Int -> [(Text, Int)]
  toSamples _ = singleSample 3

apiDocs :: API
apiDocs = docs api

writeDocs :: IO ()
writeDocs = do
  h <- IO.openFile "tmp/docs.md" IO.WriteMode
  IO.hPutStrLn h $ markdown apiDocs
  IO.hClose h

-- $> writeDocs

type Api = StartTask :<|> CheckTask

type ID = Int

type Counter = TVar ID

data Task = Task {taskData :: Int, taskId :: ID}

api :: Proxy Api
api = Proxy

app :: TQueue Task -> Counter -> Map.Map ID Status -> Application
app tasksQueue counter tasks =
  serve api $
    startTask tasksQueue counter
      :<|> checkTask tasks

main :: IO ()
main = do
  tasksQueue <- newTQueueIO
  tasks <- Map.newIO
  counter <- newTVarIO 1
  putStrLn "Server started!"
  tasksExecutor tasksQueue tasks
    `race_` run 8081 (app tasksQueue counter tasks)

startTask :: TQueue Task -> Counter -> Int -> Handler Int
startTask tasksQueue counter taskData = do
  taskId <- newId
  let task = Task{taskData, taskId}
  liftIO . atomically $ writeTQueue tasksQueue task
  liftIO $ putStrLn ("Created a task with ID " <> show taskId)
  return taskId
 where
  newId = liftIO do
    taskId <- readTVarIO counter
    atomically $ modifyTVar' counter (+ 1)
    return taskId

checkTask :: Map.Map ID Status -> Int -> Handler Status
checkTask tasks taskId = do
  status <- liftIO . atomically $ lookup taskId tasks
  case status of
    Nothing -> throwError noTaskError
    Just status_ -> return status_
 where
  noTaskError = err404{errBody = "Sorry, but there is no task with ID " <> encode taskId}

seconds :: Int -> Int
seconds n = n * 1000000

tasksExecutor :: TQueue Task -> Map.Map ID Status -> IO ()
tasksExecutor tasksQueue tasks = forever do addTask >>= runTask
 where
  addTask = atomically do
    task@(Task _ taskId) <- readTQueue tasksQueue
    Map.insert InProcess taskId tasks
    pure task
  runTask Task{..} = forkIO do
    threadDelay =<< (uniformRM ((4 `seconds`), (20 `seconds`)) globalStdGen :: IO Int)
    atomically do Map.insert (Finished (taskData * 2)) taskId tasks
