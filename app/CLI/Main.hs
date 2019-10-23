import Domain.TodoRepository
import Domain.Todo as Todo
import Data.Time (getCurrentTime, addUTCTime, nominalDay)
import Data.Text (pack)
import System.IO

data Command = Create | Show | Quit deriving (Show, Eq)

createTodo :: IO Todo
createTodo = do
  putStr "Enter a description: "
  hFlush stdout
  desc' <- getLine
  t <- getCurrentTime
  let due' = addUTCTime nominalDay t
  putStrLn "Created todo due one day from today."
  return $ Todo.create (pack desc') due'

toCommand :: String
          -> Maybe Command
toCommand "q" = Just Quit
toCommand "c" = Just Create
toCommand "s" = Just Show
toCommand _ = Nothing

getCommand :: IO Command
getCommand = do
  putStrLn "Enter a command: c (create), s (show), q (quit)"
  inStr <- getLine
  case toCommand inStr of
    Just cmd -> return cmd
    Nothing -> putStrLn "Invalid Command." >> getCommand

doCommand :: TodoRepository
          -> Command
          -> IO ()
doCommand _ Quit = return ()
doCommand repo Show = do
  todos <- getAll repo
  (putStrLn . show) todos
  loop repo
doCommand repo Create = createTodo >>= (save repo) >> loop repo

loop :: TodoRepository
     -> IO ()
loop repo = getCommand >>= doCommand repo

main :: IO ()
main = inMemoryTodoRepo >>= loop
