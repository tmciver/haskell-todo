import Domain.TodoRepository

data Command = Create | Show | Quit deriving (Show, Eq)

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

doCommand :: Command
          -> IO ()
doCommand Quit = return ()
doCommand Show = do
  todos <- getAll inMemoryTodoRepo
  (putStrLn . show) todos
  main
doCommand _ = putStrLn "Commmand not yet implemented." >> main

main :: IO ()
main = getCommand >>= doCommand
