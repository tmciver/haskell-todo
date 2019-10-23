module Domain.TodoRepository ( TodoRepository
                             , inMemoryTodoRepo
                             , getAll
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

todosSTM :: IO (TVar [Todo])
todosSTM = do
  t <- getCurrentTime
  newTVarIO [ Todo.create "Buy milk." (addUTCTime nominalDay t)
            , Todo.create "Dentist's appointment." (addUTCTime nominalDay t)
            ]

inMemoryTodoRepo :: TodoRepository
inMemoryTodoRepo = Repo { getById = \_ -> return Nothing
                        , getAll = todosSTM >>= readTVarIO
                        , save = \_ -> return ()
                        }
