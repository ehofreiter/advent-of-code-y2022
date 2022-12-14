{-# LANGUAGE TupleSections #-}
module AdventOfCodeY2022.Day14 where

import           Control.Arrow
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
dataDir = "data/day14/"

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
  let paths = map readPath lines
  -- printSample paths
  -- printSample $ paths
  --   & map pathToPts
  let rm0 = initRM paths
      maxy = maxDepth rm0
  --printRM $ (List.!! 25) $ catMaybes $ takeWhile isJust $ iterateM (createSand maxy) (Just rm0)
  let rmsPart1 = catMaybes $ takeWhile isJust $ iterateM (createSand maxy) (Just rm0)
  print $ pred $ length rmsPart1
  printRM $ last rmsPart1

  let rmsPart2 = catMaybes $ takeWhile isJust $ iterateM (createSand2 maxy) (Just rm0)
  print $ pred $ length rmsPart2
  printRM $ last rmsPart2

printRM :: RM -> IO ()
printRM = mapM_ putStrLn . showRM

showRM :: RM -> [String]
showRM rm =
  show (bounds rm)
  : [[showSpot $ Map.lookup (L.V2 x y) rm | x <- [minx..maxx]] | y <- [miny..maxy]]
  where
    L.V2 (minx, maxx) (miny, maxy) = bounds rm

showSpot :: Maybe Spot -> Char
showSpot ms = case ms of
  Nothing -> '.'
  Just R -> '#'
  Just S -> 'o'

createSand :: Int -> RM -> Maybe RM
createSand maxy rm =
  case dropSand maxy rm (L.V2 500 0) of
    Just spt -> Just $ Map.insert spt S rm
    Nothing -> Nothing

createSand2 :: Int -> RM -> Maybe RM
createSand2 maxy rm =
  case dropSand2 maxy rm (L.V2 500 0) of
    Just spt -> Just $ Map.insert spt S rm
    Nothing -> Nothing

bounds :: RM -> L.V2 (Int, Int)
bounds rm = L.V2 (minx, maxx) (miny, maxy)
  where
    ks = Map.keys rm
    minx = minimum $ map (^. L._x) ks
    maxx = maximum $ map (^. L._x) ks
    miny = minimum $ map (^. L._y) ks
    maxy = maximum $ map (^. L._y) ks

maxDepth :: RM -> Int
maxDepth rm = maximum $ map (^. L._y) $ Map.keys rm

dropSand :: Int -> RM -> Pt -> Maybe Pt
dropSand maxy rm spt
  | spt ^. L._y > maxy = Nothing -- check for lower than bottom of rocks
  | otherwise =
    let mNext = [L.V2 0 1, L.V2 (-1) 1, L.V2 1 1]
          & map (spt +)
          & map (\p -> (p, Map.lookup p rm))
          & find ((== Nothing) . snd)
          & fmap fst
    in  case mNext of
      Just nextPt -> dropSand maxy rm nextPt
      Nothing -> Just spt

dropSand2 :: Int -> RM -> Pt -> Maybe Pt
dropSand2 maxy rm spt
  | Map.lookup spt rm == Just S = Nothing -- check for totally filled
  | spt ^. L._y == maxy+1 = Just spt -- check for floor
  | otherwise =
    let mNext = [L.V2 0 1, L.V2 (-1) 1, L.V2 1 1]
          & map (spt +)
          & map (\p -> (p, Map.lookup p rm))
          & find ((== Nothing) . snd)
          & fmap fst
    in  case mNext of
      Just nextPt -> dropSand2 maxy rm nextPt
      Nothing -> Just spt

initRM :: [Path] -> RM
initRM paths = Map.fromSet (const R) $ Set.unions $ map pathToPts paths

pathToPts :: Path -> Set.Set Pt
pathToPts path = case path of
  [] -> error "bad path"
  pt:pts -> go pt pts
  where
    go pt0 pts0 = case pts0 of
      [] -> Set.empty
      pt1:pts1 ->
        let dp = signum (pt1 - pt0)
            dpts = takeWhile (/= pt1) $ iterate (+ dp) pt0
        in Set.fromList dpts <> Set.singleton pt1 <> go pt1 pts1

type RM = Map.Map Pt Spot

data Spot
  = R
  | S
  deriving (Eq, Ord, Show)

type Pt = L.V2 Int
type Path = [Pt]

readPath :: String -> Path
readPath str = str
  & splitOn " "
  & filter (/= "->")
  & map readPt

readPt :: String -> Pt
readPt str =
  let [x,y] = splitOn "," str
  in  L.V2 (read x) (read y)