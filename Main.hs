{-# LANGUAGE OverloadedStrings #-}
module Main where

import qualified Data.Text.IO as T
import Repetitions (process)

main :: IO ()
main = T.interact process >> T.putStrLn ""
