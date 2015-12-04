{-# LANGUAGE OverloadedStrings #-}
module Main where

-- import qualified Data.Text.IO as T
import System.Environment
import Web.Scotty
import Repetitions (process)

main :: IO ()
-- main = T.interact process >> T.putStrLn ""
main = do
  env <- getEnvironment
  let port = maybe 8080 read $ lookup "PORT" env
  scotty port $
    get "/" $ html "Hello World!"
