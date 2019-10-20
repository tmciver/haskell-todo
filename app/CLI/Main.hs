
data Command = Create | Show | Quit deriving (Show, Eq)

toCommand :: String
          -> Maybe Command
toCommand "q" = Just Quit
toCommand "c" = Just Create
toCommand "s" = Just Show
toCommand _ = Nothing

getCommand :: IO Command
getCommand = do
  putStrLn "Enter a command: c (create), q (quit)"
  inStr <- getLine
  case toCommand inStr of
    Just cmd -> return cmd
    Nothing -> putStrLn "Invalid Command." >> getCommand

doCommand :: Command
          -> IO ()
doCommand _ = main

main :: IO ()
main = do
  cmd <- getCommand
  putStrLn ("You entered: " ++ (show cmd))
  if cmd == Quit then
    return ()
    else doCommand cmd
