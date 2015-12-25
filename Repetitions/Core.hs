module Repetitions.Core where

import Prelude hiding (words, length)
import Text.EditDistance
import Data.Text (Text, unpack, split, toLower, length)
import qualified Data.Text as T
import qualified Data.Char as C
import Data.List (foldl')

data AnnotatedWord = Ok !Text | Bad !Text

-- |Size of neighbours list on each side of target word
constRadius :: Int
constRadius = 15

-- |Minimal distance to consider word similar
constThreshold :: Int
constThreshold = 3

-- |Minimal length of a word to use in comparison
constSignificant :: Int
constSignificant = 2

annotate :: Text -> [AnnotatedWord]
annotate = map markRepetition . weightedWords constRadius

markRepetition :: (Text, Int) -> AnnotatedWord
markRepetition (x, d)
  | d < constThreshold = Bad x
  | otherwise          = Ok x

weightedWords :: Int -> Text -> [(Text, Int)]
weightedWords radius text = map neighboursOf . indexedItems $ words
  where
    neighboursOf (w, n)
      | processable w = (w, minimum $ map (distance w) $ neighboursAt radius n words)
      | otherwise     = (w, 32)
    processable w = isSignificant $ pureWord w
    words = tokens text

tokens :: Text -> [Text]
-- tokens = T.words
tokens = split (\c -> C.generalCategory c == C.Space)

distance :: Text -> Text -> Int
distance a b = levenshteinDistance defaultEditCosts (unpack a) (unpack b)

indexedItems :: [a] -> [(a, Int)]
indexedItems xs = zip xs [0..]

neighboursAt :: Int -> Int -> [Text] -> [Text]
neighboursAt radius n ws = lastN radius (significants lefts) ++ take radius (drop 1 $ significants rights)
  where
    significants    = filter isSignificant . map pureWord
    (lefts, rights) = splitAt n ws

lastN :: Int -> [a] -> [a]
lastN n xs = foldl' (const . drop 1) xs (drop n xs)

isSignificant :: Text -> Bool
isSignificant w = length w > constSignificant

pureWord :: Text -> Text
pureWord = toLower . T.filter (not . C.isPunctuation)
