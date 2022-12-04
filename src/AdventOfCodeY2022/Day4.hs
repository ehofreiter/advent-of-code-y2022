{-# LANGUAGE TupleSections #-}
module AdventOfCodeY2022.Day4 where

import           Control.Applicative
import           Control.Lens
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
dataDir = "data/day4/"

runF, testF :: FilePath
runF = dataDir <> "input.txt"
testF = dataDir <> "test.txt"

run, test :: IO ()
run = runWith runF
test = runWith testF

runWith :: FilePath -> IO ()
runWith filePath = do
  lines <- loadLines filePath
  printSample lines
  runLines lines
  --part2 lines

loadLines :: FilePath -> IO [String]
loadLines filePath = readInputs filePath id

runLines :: [String] -> IO ()
runLines lines = do
  putStrLn "---------- part 1"
  let inputs = parseInputs lines
  printSample inputs
  print $ inputs
    & filter contained
    & length
  putStrLn "---------- part 2"
  print $ inputs
    & filter overlapped
    & length
  where
    parseInputs lines = map parsePair lines

overlapped :: Pair -> Bool
overlapped (a, b) = not $ null inter
  where
    inter = la `List.intersect` lb
    la = getList a
    lb = getList b

contained :: Pair -> Bool
contained (a, b) = inter == la || inter == lb
  where
    inter = la `List.intersect` lb
    la = getList a
    lb = getList b

getList :: Range -> [Int]
getList (L.V2 x y) = [x..y]

parsePair :: String -> Pair
parsePair s = case splitOn "," s of
  [a, b] -> (parseRange a, parseRange b)
  _ -> error "parsePair not two"

parseRange :: [Char] -> Range
parseRange s = case splitOn "-" s of
  [a, b] -> L.V2 (read a) (read b)
  _ -> error "parseRange not two"

type Pair = (Range, Range)
type Range = L.V2 Int
