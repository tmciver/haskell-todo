
data Command = Create | Show | Quit deriving (Show, Eq)

getCommand :: IO Command
getCommand = do
  putStrLn "Enter a command: q (quit)"
  _ <- getLine
  return Quit

doCommand :: IO ()
doCommand = main

main :: IO ()
main = do
  cmd <- getCommand
  if cmd == Quit then
    return ()
    else doCommand
