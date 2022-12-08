{-# LANGUAGE TupleSections #-}
module AdventOfCodeY2022.Day6 where

import           Control.Applicative
import           Control.Lens
import           Control.Monad
import           Data.Bifunctor
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

dataDir :: FilePath
dataDir = "data/day6/"

runF, testF :: FilePath
runF = dataDir <> "input.txt"
testF = dataDir <> "test.txt"

run, test :: IO ()
run = load runF >>= exec
test = load testF >>= exec

load :: FilePath -> IO [String]
load filePath = readInputs filePath id

exec :: [String] -> IO ()
exec lines = do
  let chars = head lines
  let x = zip [0..] $ map (take 4) $ List.tails chars
  let s = find ((== 4) . length . List.nub . snd) x
  print $ fmap (first (+ 4)) s
  let chars = head lines
  let x = zip [0..] $ map (take 14) $ List.tails chars
  let s = find ((== 14) . length . List.nub . snd) x
  print $ fmap (first (+ 14)) s
