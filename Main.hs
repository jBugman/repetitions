{-# LANGUAGE OverloadedStrings #-}
module Main where

import System.Environment
import Web.Scotty
import Text.Blaze.Html.Renderer.Text
import Text.Blaze.Html5 (Html)

-- import Repetitions.Core (process)
import Repetitions.Frontend (index)

main :: IO ()
main = do
  env <- getEnvironment
  let port = maybe 8080 read $ lookup "PORT" env
  scotty port $
    get "/" $ render index

render :: Html -> ActionM ()
render = html . renderHtml
