module Main where

import Data.Text (Text)
import System.Environment
import Network.Wai.Middleware.RequestLogger
import Web.Scotty

import Repetitions.Core (annotate)
import Repetitions.Frontend (index, result)


main :: IO ()
main = do
  env <- getEnvironment
  let port = maybe 8080 read $ lookup "PORT" env
  scotty port $ do
    middleware logStdoutDev

    get "/" $ html index
    post "/result" $ (param "text" :: ActionM Text) >>= (html . result . annotate)
