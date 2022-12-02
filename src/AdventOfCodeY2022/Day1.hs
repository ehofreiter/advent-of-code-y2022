{-# LANGUAGE TupleSections #-}
module AdventOfCodeY2022.Day1 where

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

runF = "data/day1/input.txt"
testF = "data/day1/test.txt"

run = runWith runF
test = runWith testF

runWith :: FilePath -> IO ()
runWith filePath = do
  inputs <- load filePath
  -- print inputs
  let elfCals = map (map read) inputs :: [[Int]]
      elfTotalCals = map sum elfCals
      part1 = maximum elfTotalCals
  print part1
  let part2 = sum $ take 3 $ reverse $ List.sort elfTotalCals
  print part2

load :: FilePath -> IO [[String]]
load filePath = do
  strs <- readInputs filePath id
  pure $ splitOn [[]] strs
