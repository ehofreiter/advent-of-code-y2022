{-# LANGUAGE TupleSections #-}
module AdventOfCodeY2022.Day3 where

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
dataDir = "data/day3/"


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
  print $ take 10 inputs
  print $ length inputs
  let cc = map (charToPri . commonChar) inputs
  print $ take 10 cc
  print $ sum cc

  let inputs2 = parseInputs2 lines
  print $ length inputs2
  print $ take 10 inputs2
  let cc = map (charToPri . commonChar2) inputs2
  print $ take 10 cc
  print $ sum cc

loadLines :: FilePath -> IO [String]
loadLines filePath = readInputs filePath id

parseInputs :: [String] -> [(String,String)]
parseInputs strs = f <$> strs
  where
    f s = splitAt (length s `div` 2) s

commonChar :: (String, String) -> Char
commonChar (x, y) =
  let mapX = mkHistogram x
      mapY = mkHistogram y
  in  fst $ head $ Map.toList $ Map.intersection mapX mapY

charToPri :: Char -> Int
charToPri c | 'a' <= c && c <= 'z' = ord c - ord 'a' + 1
            | otherwise = ord c - ord 'A' + 27

parseInputs2 :: [String] -> [[String]]
parseInputs2 = chunksOf 3

commonChar2 :: [String] -> Char
commonChar2 ss = fst $ head $ Map.toList $ foldr1 Map.intersection $ map mkHistogram ss
