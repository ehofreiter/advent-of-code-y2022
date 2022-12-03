{-# LANGUAGE TupleSections #-}
module AdventOfCodeY2022.NewDay where

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

dataDir :: FilePath
dataDir = "data/dayX/"

runF, testF :: FilePath
runF = dataDir <> "input.txt"
testF = dataDir <> "test.txt"

run, test :: IO ()
run = runWith runF
test = runWith testF

runWith :: FilePath -> IO ()
runWith filePath = do
  lines <- loadLines filePath
  print $ take 10 lines
  print $ length lines
  let inputs = parseInputs lines
  print $ length inputs

loadLines :: FilePath -> IO [String]
loadLines filePath = readInputs filePath id

parseInputs :: [String] -> [String]
parseInputs = id