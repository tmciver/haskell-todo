{-# LANGUAGE OverloadedStrings #-}

import Web.Scotty
import Domain.Todo
import Domain.TodoRepository
import Data.Text as T
import Control.Monad (forM_)
import Control.Monad.IO.Class
import Data.String (fromString)
import qualified Text.Blaze.Html5 as H
import Text.Blaze.Html5.Attributes
import Text.Blaze.Html.Renderer.Text (renderHtml)

env :: ActionM TodoRepository
env = liftIO inMemoryTodoRepo

todosHtml :: [Todo] -> H.Html
todosHtml todos = do
  H.p "Todos:"
  H.ul $ forM_ todos (H.li . fromString . T.unpack . desc)

homeLink :: H.Html
homeLink = ((H.a . H.toHtml) ("Home" :: String)) H.! href "/"

template :: String -> H.Html -> H.Html
template title' body' = H.docTypeHtml $ do
  H.head $ do
    H.title $ fromString $ "Haskell Todo - " ++ title'
  H.body $ homeLink >> H.br >> body'

homeView :: ActionM ()
homeView = do
  repo <- env
  todos <- liftIO $ getAll repo
  html $ renderHtml $ homeHtml todos

homeHtml :: [Todo] -> H.Html
homeHtml todos = template "Home" (todosHtml todos)

main :: IO ()
main = scotty 3000 $ do
  get "/" homeView
