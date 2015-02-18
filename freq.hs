#!/usr/bin/runhaskell

import Data.Char (isAsciiLower, isAsciiUpper, toLower)
import Data.Function (on)
import Data.List (sortBy)

import qualified Data.Map.Strict as Map

data Bigram = B Char Char
  deriving (Eq, Ord)

data Trigram = T Char Char Char
  deriving (Eq, Ord)

instance Show Bigram where
  show (B x y) = show [x,y]

instance Show Trigram where
  show (T x y z) = show [x,y,z]

main = interact $ \x -> unlines $
  let y = normalize x in
  ["== letters =="]       ++ (freqTable $ letterFreq y) ++
  ["", "== bigrams =="]   ++ (freqTable $ bigramFreq y) ++
  ["", "== trigrams =="]  ++ (freqTable $ trigramFreq y)

normalize = map toLower . filter (\x -> isAsciiLower x || isAsciiUpper x)

freqTable m
  = map (\x -> (show $ fst x) ++ " -> " ++ (show $ snd x))
  $ reverse $ sortBy (compare `on` snd) $ Map.toList m

letterFreq :: String -> Map.Map Char Int
letterFreq s = f s Map.empty
  where
    f (x:xs)  m = f xs $ Map.insertWith (+) x 1 m
    f _       m = m

bigramFreq :: String -> Map.Map Bigram Int
bigramFreq s = f s Map.empty
  where
    f (x:xs@(y:_))  m = f xs $ Map.insertWith (+) (B x y) 1 m
    f _             m = m

trigramFreq :: String -> Map.Map Trigram Int
trigramFreq s = f s Map.empty
  where
    f (x:xs@(y:z:_))  m = f xs $ Map.insertWith (+) (T x y z) 1 m
    f _               m = m
