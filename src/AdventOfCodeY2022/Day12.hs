{-# LANGUAGE TupleSections #-}
module AdventOfCodeY2022.Day12 where

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
dataDir = "data/day12/"


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
  let cm = CV.fromLists lines
  let hm = readNode <$> cm
  let sc = startCoord cm
  let ec = endCoord cm
  print $ startCoord cm
  print $ endCoord cm
  --print hm
  let starts = getAllAs cm
  --print starts
  putStrLn "part1"
  print $ getBest sc ec hm
  putStrLn "part2"
  print $ head $ List.sort $ mapMaybe (\s -> getBest s ec hm) starts
  --mapM_ print $ minCostSearch (costNbors hm) sc

getAllAs :: CV.CoordVec Char -> [CV.Coord]
getAllAs cv = CV.unflattenCoord cv <$> (List.elemIndices 'a' $ CV.toList cv)

getBest :: CV.Coord -> CV.Coord -> CV.CoordVec Int -> Maybe (Int, CV.Coord)
getBest sc ec hm = do
  (d, _) <- listToMaybe $ dropWhile ((/= ec) . snd) $ minCostSearch (costNbors hm) sc
  pure (d, sc)

costNbors :: CV.CoordVec Int -> CV.Coord -> [(Int, CV.Coord)]
costNbors cv n =
  let nbors = filter f $ CV.adjPairs4 cv n
      f (n, h) = (h - thisH) <= 1
      thisH = cv CV.! n
  in  nbors & map (\(n, h) -> (1, n))

startCoord :: CV.CoordVec Char -> CV.Coord
startCoord cv = CV.unflattenCoord cv $ fromJust $ List.elemIndex 'S' $ CV.toList cv

endCoord :: CV.CoordVec Char -> CV.Coord
endCoord cv = CV.unflattenCoord cv $ fromJust $ List.elemIndex 'E' $ CV.toList cv

readNode :: Char -> Int
readNode c = ord c' - ord 'a'
  where
    c' = case c of
      'S' -> 'a'
      'E' -> 'z'
      _ -> c