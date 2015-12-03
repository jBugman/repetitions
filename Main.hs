{-# LANGUAGE OverloadedStrings #-}
module Main where

import Text.EditDistance
import qualified Data.Char as C
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.List (foldl')

-- |Size of neighbours list on each side of target word
constRadius :: Int
constRadius = 15

-- |Minimal distance to consider word similar
constThreshold :: Int
constThreshold = 3

-- |Minimal length of a word to use in comparison
constSignificant :: Int
constSignificant = 2

main :: IO ()
main = T.interact process >> T.putStrLn ""

process :: T.Text -> T.Text
process text = T.unwords $ map (significantCase constThreshold) $ weightedWords constRadius text

weightedWords :: Int -> T.Text -> [(T.Text, Int)]
weightedWords radius text = map neighboursOf $ indexedItems ws where
  neighboursOf (w, n)
    | processable w = (w, minimum $ map (distance w) $ neighboursAt radius n ws)
    | otherwise     = (w, 32)
  processable w = isSignificant $ pureWord w
  ws = T.words text

distance :: T.Text -> T.Text -> Int
distance a b = levenshteinDistance defaultEditCosts (T.unpack a) (T.unpack b)

indexedItems :: [a] -> [(a, Int)]
indexedItems xs = zip xs [0..]

significantCase :: Int -> (T.Text, Int) -> T.Text
significantCase threshold (x, d)
  | d < threshold = T.toUpper x
  | otherwise     = x

neighboursAt :: Int -> Int -> [T.Text] -> [T.Text]
neighboursAt radius n ws = lastN radius (significants lefts) ++ take radius (drop 1 $ significants rights)
  where
    significants xs = filter isSignificant $ map pureWord xs
    (lefts, rights) = splitAt n ws

lastN :: Int -> [a] -> [a]
lastN n xs = foldl' (const . drop 1) xs (drop n xs)

isSignificant :: T.Text -> Bool
isSignificant w = T.length w > constSignificant

pureWord :: T.Text -> T.Text
pureWord word = T.toLower $ T.filter (not . C.isPunctuation) word
