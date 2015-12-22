{-# LANGUAGE OverloadedStrings #-}
module Main where

import Data.Text (Text, unwords, toUpper)
import Text.Blaze.Html5 (Html)

import System.Environment
import Network.Wai.Middleware.RequestLogger
import Web.Scotty
import Text.Blaze.Html.Renderer.Text
import Text.Blaze.Html (toHtml)

import Repetitions.Core
import Repetitions.Frontend (index, result)


main :: IO ()
main = do
  env <- getEnvironment
  let port = maybe 8080 read $ lookup "PORT" env
  scotty port $ do
    middleware logStdoutDev

    get "/" $ render index

    post "/process" $ do
      t <- param "text" :: ActionM Text
      let r = process t
      let h = toHtml r
      render $ result h

render :: Html -> ActionM ()
render = html . renderHtml

process :: Text -> Text
process t = Data.Text.unwords $ map significantCase $ annotate t
  where
    significantCase (Ok t) = t
    significantCase (Bad t) = toUpper t
