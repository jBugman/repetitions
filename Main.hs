module Main where

import qualified Data.Char as C

main :: IO ()
-- main = interact ...
main = do
  text <- readFile "test.txt"
  putStrLn $ process text

process :: String -> String
process input =
  unwords $
  filter (\x -> length x > 2) $
  words $
  wordsOnly input

wordsOnly :: String -> String
wordsOnly xs = map C.toLower $ filter (not . C.isPunctuation) xs
