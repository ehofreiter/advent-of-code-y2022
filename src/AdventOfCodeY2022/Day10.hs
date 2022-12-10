{-# LANGUAGE TupleSections #-}
module AdventOfCodeY2022.Day10 where

import           Control.Applicative
import           Control.Lens
import           Control.Monad
import           Data.Bifunctor
import           Data.Char
import           Data.Foldable
import           Data.Foldable.WithIndex
import           Data.Functor
import           Data.Functor.WithIndex
import qualified Data.List as List
import           Data.List.Split
import           Data.Maybe
import           Data.Traversable
import           Data.Traversable.WithIndex
import qualified Data.Map.Strict as Map
import qualified Data.IntMap.Strict as IntMap
import qualified Data.Sequence as Seq
import           Data.Sequence (Seq((:|>), (:<|)), (><))
import qualified Data.Set as Set
import qualified Linear as L
import qualified Text.Parsec as P

import AdventOfCodeY2022.Common
import qualified AdventOfCodeY2022.CoordVec as CV

-- $> run

-- $> test

dataDir :: FilePath
dataDir = "data/day10/"

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
  -- printSample lines
  let inputs = map readInstr lines
  let states = inputs & doProg

  mapM_ print $ states & everyNthCycleFrom 20 40 & map (uncurry (*))
  print $ strengthAt 20 states
  printSample $ streamCycles states
  print $ last states
  mapM_ putStrLn $ runCrt states

runCrt :: [SimState] -> [String]
runCrt states = chunksOf 40 $ map drawPixel $ streamCycles states

drawPixel :: SimState -> Char
drawPixel (cycle, x)
  | abs (x - crtPos) <= 1 = '#'
  | otherwise = '.'
  where
    crtPos = cycle `mod` 40

streamCycles :: [SimState] -> [SimState]
streamCycles = go 0 1
  where
    go cycle x states = case states of
      [] -> []
      (sc, sx):ss
        | cycle < sc -> (cycle, x) : go (cycle+1) x states
        | otherwise -> (cycle, sx) : go (cycle+1) sx ss

strengthAt :: Int -> [SimState] -> Int
strengthAt n = sum . take 6 . map (uncurry (*)) . everyNthCycleFrom n (2*n)

atCycle :: Int -> [SimState] -> SimState
atCycle n = head . dropWhile ((< n) . fst)

everyNthCycleFrom :: Int -> Int -> [SimState] -> [SimState]
everyNthCycleFrom from n ss = go from ss
  where
    go cycle states = case earlier of
      [] -> []
      _ -> first (const cycle) (last earlier) : go (cycle + n) later
      where
        (earlier, later) = span ((<= cycle) . fst) states

doProg :: [Instr] -> [SimState]
doProg = List.scanl' doInstr initState

initState :: SimState
initState = (0, 1)

type SimState = (Cycle, CpuState)

doInstr :: SimState -> Instr -> SimState
doInstr (c, x) instr = case instr of
  Addx v -> (c+2, x + v)
  Noop -> (c+1, x)

readInstr :: String -> Instr
readInstr str = case splitOn " " str of
  ["addx", vstr] -> Addx (read vstr)
  ["noop"] -> Noop
  _ -> error "readInstr"

data Instr = Addx Int | Noop
  deriving (Eq, Ord, Show)

type CpuState = Int
type Cycle = Int