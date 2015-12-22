{-# LANGUAGE OverloadedStrings #-}
module Main where

import Data.Text (Text, unwords, toUpper)
import Text.Blaze.Html5 (Html)

import System.Environment
import Network.Wai.Middleware.RequestLogger
import Web.Scotty
import Text.Blaze.Html.Renderer.Text
import Text.Blaze.Html (toHtml)

import Repetitions.Core (annotate, AnnotatedWord(..))
import Repetitions.Frontend (index, result)


main :: IO ()
main = do
  env <- getEnvironment
  let port = maybe 8080 read $ lookup "PORT" env
  scotty port $ do
    middleware logStdoutDev

    get "/" $ render index
    post "/process" $ (param "text" :: ActionM Text) >>= (render . result . toHtml . process)


render :: Html -> ActionM ()
render = html . renderHtml

process :: Text -> Text
process = Data.Text.unwords . map badToUpper . annotate
  where
    badToUpper (Ok x) = x
    badToUpper (Bad x) = toUpper x
