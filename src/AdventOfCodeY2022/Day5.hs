{-# LANGUAGE TupleSections #-}
module AdventOfCodeY2022.Day5 where

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
dataDir = "data/day5/"

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
  printSample lines
  let (stacks, steps) = case take 2 $ splitOn [""] lines of
        [stk, stp] -> (stk, stp)
        _ -> error "halp"
  let stackList = stacks
        & List.transpose
        & map reverse
        & filter (isDigit . head)
        & map (reverse . tail . takeWhile (/= ' '))
  mapM_ putStrLn stackList
  let stepList = steps
        & map (map (read :: String -> Int) . filter (isDigit . head) . splitOn " ")
      stackMap = Map.fromList $ zip [1..] stackList
      finalStack = foldl' doStep stackMap stepList
      finalStack2 = foldl' doStep2 stackMap stepList
  --print $ map (head . snd) $ Map.toList finalStack
  print $ map (head . snd) $ Map.toList finalStack2

doStep :: Map.Map Int String -> [Int] -> Map.Map Int String
doStep stacks [n, from, to] =
  foldr ($) stacks (replicate n (move1' from to))
doStep _ stacks = error "no"

doStep2 :: Map.Map Int String -> [Int] -> Map.Map Int String
doStep2 stacks [n, from, to] =
  let fromStack = stacks Map.! from
      toStack = stacks Map.! to
  in  Map.insert to (take n fromStack <> toStack) $ Map.insert from (drop n fromStack) stacks
doStep2 _ stacks = error "no"

move1 :: Int -> Int -> Map.Map Int String -> Map.Map Int String
move1 from to stacks =
  let fromStack = stacks Map.! from
      toStack = stacks Map.! to
  in  case fromStack of
    a:fromStack' -> Map.insert to (a:toStack) $ Map.insert from fromStack' stacks
    [] -> stacks

move1' :: Int -> Int -> Map.Map Int String -> Map.Map Int String
move1' from to stacks =
  let fromStack = stacks Map.! from
      toStack = stacks Map.! to
  in  case fromStack of
    a:fromStack' -> Map.insert to (a:toStack) $ Map.insert from fromStack' stacks
    [] -> error $ unlines $ map show $ Map.toList stacks