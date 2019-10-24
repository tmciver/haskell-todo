module Domain.Todo where

import Data.Text
import Data.Time
import Data.UUID
import Data.UUID.V4 (nextRandom)

data Status = New | InProgress | Completed deriving (Eq, Show)

data Todo = Todo { getId :: UUID
                 , desc :: Text
                 , status :: Status
                 , due :: UTCTime
                 } deriving (Eq, Show)

create :: Text
       -> UTCTime
       -> IO Todo
create desc due = do
  uuid <- nextRandom
  pure $ Todo { getId = uuid, desc = desc, status = New, due = due }

start :: Todo
      -> Maybe Todo
start todo@(Todo _ _ New _) = Just $ todo { status = InProgress }
start _ = Nothing

complete :: Todo
         -> Maybe Todo
complete todo@(Todo _ _ InProgress _) = Just $ todo { status = Completed }
complete _ = Nothing
