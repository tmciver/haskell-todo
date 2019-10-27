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
import Data.Time (getCurrentTime, addUTCTime, nominalDay)

todosHtml :: [Todo] -> H.Html
todosHtml todos = do
  H.p "Todos:"
  H.ul $ forM_ todos (H.li . fromString . T.unpack . desc)

todoForm :: H.Html
todoForm = H.form H.! method "post" H.! enctype "multipart/form-data" H.! action "/todos" $ do
  H.span $ H.toHtml ("Enter a description:" :: Text)
  H.br
  H.input H.! type_ "textarea" H.! name "desc"
  H.br
  H.input H.! type_ "submit" H.! value "Submit"

createTodo :: TodoRepository
           -> Text
           -> ActionM ()
createTodo repo desc' = do
  t <- liftIO getCurrentTime
  let due' = addUTCTime nominalDay t
  todo <- liftIO (create desc' due')
  liftIO (save repo todo)
  redirect "/"

handleTodoPost :: TodoRepository -> ActionM ()
handleTodoPost repo = param "desc" >>= createTodo repo

homeLink :: H.Html
homeLink = ((H.a . H.toHtml) ("Home" :: String)) H.! href "/"

template :: String -> H.Html -> H.Html
template title' body' = H.docTypeHtml $ do
  H.head $ do
    H.title $ fromString $ "Haskell Todo - " ++ title'
  H.body $ homeLink >> H.br >> body'

homeView :: TodoRepository -> ActionM ()
homeView repo = do
  todos <- liftIO $ getAll repo
  html $ renderHtml $ homeHtml todos

homeHtml :: [Todo] -> H.Html
homeHtml todos = template "Home" todoForm >> (todosHtml todos)

main :: IO ()
main = do
  repo <- liftIO inMemoryTodoRepo
  scotty 3000 $ do
    get "/" (homeView repo)
    post "/todos" (handleTodoPost repo)
