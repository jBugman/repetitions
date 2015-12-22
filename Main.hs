{-# LANGUAGE OverloadedStrings #-}
module Main where

import Data.Text (Text)
import Text.Blaze.Html5 (Html)

import System.Environment
import Network.Wai.Middleware.RequestLogger
import Web.Scotty
import Text.Blaze.Html.Renderer.Text (renderHtml)

import Repetitions.Core (annotate)
import Repetitions.Frontend (index, result)


main :: IO ()
main = do
  env <- getEnvironment
  let port = maybe 8080 read $ lookup "PORT" env
  scotty port $ do
    middleware logStdoutDev

    get "/" $ render index
    post "/process" $ (param "text" :: ActionM Text) >>= (render . result . annotate)


render :: Html -> ActionM ()
render = html . renderHtml
