{-# LANGUAGE OverloadedStrings #-}

import Domain.TodoRepository
import Domain.Todo as Todo
import Data.Time (getCurrentTime, addUTCTime, nominalDay)
import Data.Text (Text, pack)
import System.IO
import Pipes
import qualified Pipes.Prelude as P

-- Pipes stuff

commandProducer :: Producer Command IO ()
commandProducer = do
  lift $ putStrLn "Enter a command: c (create), s (show); (Ctl-D to quit)"
  for P.stdinLn (\str -> do
                    case toCommand str of
                      Just cmd -> yield cmd
                      Nothing -> (lift $ putStrLn "Invalid Command.") >> (lift $ hFlush stdout) >> commandProducer)

printerConsumer :: Show a
                => a
                -> Effect IO ()
printerConsumer = lift . putStrLn . show

-- end Pipes stuff

data Command = Create | Show deriving (Show, Eq)

getDescription :: IO Text
getDescription = do
  putStr "Enter a description: "
  hFlush stdout
  desc' <- getLine
  if null desc' then
    putStrLn "Description must not be empty." >> getDescription
    else return $ pack desc'

createTodo :: IO Todo
createTodo = do
  desc' <- getDescription
  t <- getCurrentTime
  let due' = addUTCTime nominalDay t
  putStrLn "Created todo due one day from today."
  Todo.create desc' due'

toCommand :: String
          -> Maybe Command
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
doCommand repo Show = do
  todos <- getAll repo
  (putStrLn . show) todos
  loop repo
doCommand repo Create = createTodo >>= (save repo) >> loop repo

loop :: TodoRepository
     -> IO ()
loop repo = getCommand >>= doCommand repo

main :: IO ()
--main = inMemoryTodoRepo >>= loop
main = runEffect $ for commandProducer printerConsumer
