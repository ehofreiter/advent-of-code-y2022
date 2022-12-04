{-# LANGUAGE TupleSections #-}
module AdventOfCodeY2022.Day2 where

import           Control.Applicative
import           Control.Monad
import           Data.Char
import           Data.Foldable
import qualified Data.List as List
import qualified Linear as L
import           Data.List.Split
import           Data.Maybe
import           Data.Traversable
import qualified Data.Map.Strict as Map
import qualified Data.Sequence as Seq
import           Data.Sequence (Seq((:|>), (:<|)), (><))
import qualified Data.Set as Set
import qualified Text.Parsec as P

import AdventOfCodeY2022.Common

-- $> run

-- $> test

runF = "data/day2/input.txt"
testF = "data/day2/test.txt"

run = runWith runF
test = runWith testF

runWith :: FilePath -> IO ()
runWith filePath = do
  inputs <- load filePath
  let scores = map scorePair2 inputs
  --print scores
  print $ sum scores

load :: FilePath -> IO [(RPS, RPS)]
load filePath = do
  strs <- readInputs filePath $ \str -> case splitOn " " str of
    [a, b] -> (readRps1 a, readRps2 b)
    _ -> error "not two"
  pure strs
data RPS = Ro | Pa | Sc deriving (Eq, Ord, Show)

readRps1 :: String -> RPS
readRps1 c = case c of
  "A" -> Ro
  "B" -> Pa
  "C" -> Sc
  _ -> error "hi"

readRps2 :: String -> RPS
readRps2 c = case c of
  "X" -> Ro
  "Y" -> Pa
  "Z" -> Sc
  _ -> error "hi"

scoreRps :: RPS -> Int
scoreRps r = case r of
  Ro -> 1
  Pa -> 2
  Sc -> 3

scorePair :: (RPS, RPS) -> Int
scorePair (a, b) = scoreRps b + scoreWin (a, b)

scorePair2 :: (RPS, RPS) -> Int
scorePair2 (a, b) =
  let (w, r2) = case (a, b) of
        (Ro, Ro) -> (0, Sc)
        (Ro, Pa) -> (3, Ro)
        (Ro, Sc) -> (6, Pa)
        (Pa, Ro) -> (0, Ro)
        (Pa, Pa) -> (3, Pa)
        (Pa, Sc) -> (6, Sc)
        (Sc, Ro) -> (0, Pa)
        (Sc, Pa) -> (3, Sc)
        (Sc, Sc) -> (6, Ro)
  in  scoreRps r2 + w

scoreWin :: (RPS, RPS) -> Int
scoreWin (a, b) = case (a, b) of
  (Ro, Ro) -> 3
  (Ro, Pa) -> 6
  (Ro, Sc) -> 0
  (Pa, Ro) -> 0
  (Pa, Pa) -> 3
  (Pa, Sc) -> 6
  (Sc, Ro) -> 6
  (Sc, Pa) -> 0
  (Sc, Sc) -> 3
