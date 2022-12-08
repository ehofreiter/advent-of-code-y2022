{-# LANGUAGE TupleSections #-}
module AdventOfCodeY2022.Day8 where

import           Control.Applicative
import           Control.Lens as Lens hiding (imap)
import           Control.Monad
import           Data.Bifunctor
import           Data.Char
import           Data.Foldable
import           Data.Functor.WithIndex
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
import qualified AdventOfCodeY2022.CoordVec as CV

-- $> run

-- $> test

dataDir :: FilePath
dataDir = "data/day8/"

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
  let trees = readTrees lines
  --printSample trees
  let rowIxs = foldMap visibleIxs trees
  let colIxs = foldMap visibleIxs (List.transpose trees)
  let rowIxsRev = foldMap visibleIxs (map reverse trees)
  let colIxsRev = foldMap visibleIxs (map reverse $ List.transpose trees)
  print $ Set.size $ rowIxs <> colIxs <> rowIxsRev <> colIxsRev
  let treeCV = readTreeCV lines
  let scores = imap (\c t -> viewScore treeCV c) treeCV
  print $ maximum $ CV.toList scores

viewScore :: CV.CoordVec Tree -> CV.Coord -> Int
viewScore cvt c = upScore * leftScore * rightScore * downScore
  where
    upScore = dirScore cvt c (L.V2 0 (-1))
    downScore = dirScore cvt c (L.V2 0 1)
    leftScore = dirScore cvt c (L.V2 (-1) 0)
    rightScore = dirScore cvt c (L.V2 1 0)

dirScore :: CV.CoordVec Tree -> CV.Coord -> CV.Coord -> Int
dirScore cvt c dir = go (c + dir)
  where
    thisTree = cvt CV.! c
    go c' = case cvt CV.!? c' of
      Nothing -> 0
      Just t' | t' < thisTree -> 1 + go (c' + dir)
              | otherwise     -> 1

readTreeCV :: [String] -> CV.CoordVec Tree
readTreeCV = CV.fromLists . (map.map) (read . (:[]))

readTrees :: [String] -> [[((Int, Int), Tree)]]
readTrees lines =
  [ [ ((x, y), read [st]) | (x, st) <- zip [0..] l ] | (y, l) <- zip [0..] lines ]

type Tree = Int

visibleIxs :: Ord ix => [(ix, Tree)] -> Set.Set ix
visibleIxs = go (-1)
  where
    go tallest ts = case ts of
      [] -> Set.empty
      (i, t):ts' -> if t > tallest
                    then Set.insert i $ go t ts'
                    else go tallest ts'