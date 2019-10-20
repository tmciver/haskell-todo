{-# LANGUAGE OverloadedStrings #-}

import Web.Scotty
import Data.Text as T
import Data.Text.Lazy (toStrict)

main :: IO ()
main = scotty 3000 $ do
  get "/:word" $ do
    word <- param "word"
    let beam = if T.null (toStrict word) then "beam" else word
    html $ mconcat ["<h1>Scotty, ", beam, " me up!</h1>"]
