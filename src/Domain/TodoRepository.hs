module Domain.TodoRepository ( TodoRepository
                             , inMemoryTodoRepo
                             , getAll
                             , save
                             ) where

import Domain.Todo as Todo
import Data.Time (getCurrentTime, addUTCTime, nominalDay)
import Control.Concurrent.STM

type ID = String

data Repository i a = Repo { getById :: i -> IO (Maybe a)
                           , getAll :: IO [a]
                           , save :: a -> IO ()
                           }

type TodoRepository = Repository ID Todo

inMemoryTodoRepo :: IO TodoRepository
inMemoryTodoRepo = do
  t <- getCurrentTime
  todosTVar <- newTVarIO []
  let repo = Repo { getById = \_ -> return Nothing
                  , getAll = readTVarIO todosTVar
                  , save = \todo -> atomically $ modifyTVar' todosTVar (todo:)
                  }
  save repo (Todo.create "Buy milk." (addUTCTime nominalDay t))
  save repo (Todo.create "Dentist's appointment." (addUTCTime nominalDay t))
  return repo
