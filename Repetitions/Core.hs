module Repetitions.Core where

import Prelude hiding (words, length, lines)
import Text.EditDistance
import Data.List (intercalate, foldl')
import Data.Text (Text, unpack, split, toLower, length, lines)
import qualified Data.Text as T
import qualified Data.Char as C

data AnnotatedWord = Ok !Text | Bad !Text | OkLF deriving (Show, Eq)
data Token = Word !Text | LF deriving Show

isWord :: Token -> Bool
isWord (Word _) = True
isWord LF       = False

-- |Size of neighbours list on each side of target word
constRadius :: Int
constRadius = 15

-- |Minimal distance to consider word similar
constThreshold :: Int
constThreshold = 3

-- |Minimal length of a word to use in comparison
constSignificant :: Int
constSignificant = 2

-- |Means that two words are totally different
infiniteDistance :: Int
infiniteDistance = 9001

annotate :: Text -> [AnnotatedWord]
annotate = map markRepetition . weightedWords

markRepetition :: (Token, Int) -> AnnotatedWord
markRepetition (LF, _) = OkLF
markRepetition (Word word, weight)
  | similar   = Bad word
  | otherwise = Ok word
    where
      similar = weight < constThreshold

weightedWords :: Text -> [(Token, Int)]
weightedWords text = map neighboursOf . indexedItems $ words
  where
    neighboursOf (w, n)
      | isSignificant w' = (w, minimum $ map distance'' $ neighboursAt constRadius n words)
      | otherwise        = (w, infiniteDistance)
        where
          distance'' x = distance w' $ pureWord' x
          w' = pureWord' w
    words = tokens text

tokens :: Text -> [Token]
tokens = intercalate [LF] . map (map Word . tokens') . filter (not . T.null) . lines
  where
    tokens' = split (\c -> C.generalCategory c == C.Space)
-- tokens = intercalate [LF] . map (map Word . tokens') . filter (not . T.null) . lines
--   where
--     tokens' = filter (not . T.null) . split (\c -> C.generalCategory c == C.Space)

distance :: Token -> Token -> Int
distance LF _ = infiniteDistance
distance _ LF = infiniteDistance
distance (Word a) (Word b) = distance' a b

distance' :: Text -> Text -> Int
distance' a b = levenshteinDistance defaultEditCosts (unpack a) (unpack b)

indexedItems :: [a] -> [(a, Int)]
indexedItems xs = zip xs [0..]

neighboursAt :: Int -> Int -> [Token] -> [Token]
neighboursAt radius n ws = lastN radius (significants lefts) ++ take radius (drop 1 $ significants rights)
  where
    significants       = filter isSignificant . map pureWord'
    (lefts, rights)    = splitAt n ws

lastN :: Int -> [a] -> [a]
lastN n xs | n > 0     = foldl' (const . drop 1) xs (drop n xs)
lastN _ _  | otherwise = []

isSignificant :: Token -> Bool
isSignificant LF       = False
isSignificant (Word w) = length w > constSignificant

pureWord' :: Token -> Token
pureWord' LF       = LF
pureWord' (Word w) = Word $ pureWord w

pureWord :: Text -> Text
pureWord = toLower . T.filter (not . isLetter)
  where
    isLetter c = C.isPunctuation c || C.isSeparator c
