module Domain.Todo where

import Data.Text
import Data.Time

data Status = New | InProgress | Completed deriving (Eq, Show)

data Todo = Todo { desc :: Text
                 , status :: Status
                 , due :: UTCTime
                 } deriving (Eq, Show)

create :: Text
       -> UTCTime
       -> Todo
create desc due = Todo { desc = desc, status = New, due = due }

start :: Todo
      -> Maybe Todo
start (Todo desc New due) = Just $ Todo { desc = desc, status = InProgress, due = due }
start _ = Nothing

complete :: Todo
         -> Maybe Todo
complete (Todo desc InProgress due) = Just $ Todo { desc = desc, status = Completed, due = due }
complete _ = Nothing
