{-# LANGUAGE TupleSections #-}
module AdventOfCodeY2022.Day18 where

import           Control.Applicative
import           Control.Lens
import           Control.Monad
import           Data.Bifunctor
import           Data.Char
import           Data.Either
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
dataDir = "data/day18/"

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
  let cubeSet = Set.fromList $ map readCubeP lines
  --printSample $ Set.toList cubeSet
  putStrLn "--part1"
  --print $ surface cubeSet
  putStrLn "--part2"
  -- print $ boundsV $ Set.toList cubeSet
  print $ outerSurface cubeSet
  -- let bds0 = boundsV (Set.toList cubeSet)
  --     bds = expand <$> bds0
  --     p0 = fst <$> bds
  -- print bds
  -- let printResult = either print printFillState
  -- mapM_ (\(n, r) -> do
  --   putStrLn $ "-- STEP " <> show n <> " --"
  --   printResult r
  --   )
  --   $ take 5
  --   $ drop 199
  --   -- $ dropWhile (isRight . snd)
  --   $ zip [0..]
  --   $ iterateM (stepFill cubeSet bds) (Right $ initFillState bds)

printFillState :: FillState -> IO ()
printFillState (next, seen, area) = do
  putStrLn "--"
  putStrLn $ "next: " <> show next
  putStrLn $ "seen: " <> show seen
  putStrLn $ "seen count: " <> show (Set.size seen)
  putStrLn $ "area: " <> show area

type FillState = (Set.Set (L.V3 Int), Set.Set (L.V3 Int), Int)

initFillState :: L.V3 (Int, Int) -> FillState
initFillState bds = (Set.singleton p0, Set.empty, 0)
  where
    p0 = fst <$> bds

stepFill :: CubeSet -> L.V3 (Int, Int) -> FillState -> Either Int FillState
stepFill cs bds (next, seen, area) = case Set.minView next of
  Nothing -> Left area
  Just (p, next') -> Right (next'', seen', area')
    where
      next'' = next' <> Set.fromList emptyNbors
      seen' = Set.insert p seen
      area' = area + length lavaNbors
      (lavaNbors, emptyNbors) = List.partition (`Set.member` cs) unseenNbors
      unseenNbors
        = filter (inBoundsV bds)
        $ filter (`Set.notMember` seen)
        $ getNbors p

outerSurface :: CubeSet -> Int
outerSurface cs = fill (Set.singleton p0) Set.empty 0
  where
    bds = expand <$> boundsV (Set.toList cs)
    p0 = fst <$> bds
    fill next seen area = case Set.minView next of
      Nothing -> area
      Just (p, next') -> fill next'' seen' area'
        where
          next'' = next' <> Set.fromList emptyNbors
          seen' = Set.insert p seen
          area' = area + length lavaNbors
          -- ugh, initially thought `List.partition` was called `span`, took me
          -- a long time to catch the bug
          (lavaNbors, emptyNbors) = List.partition (`Set.member` cs) unseenNbors
          unseenNbors
            = filter (inBoundsV bds)
            $ filter (`Set.notMember` seen)
            $ getNbors p

getNbors :: L.V3 Int -> [L.V3 Int]
getNbors v = [v + dv | dv <- [dx,-dx,dy,-dy,dz,-dz]]
  where
    dx = L.V3 1 0 0
    dy = L.V3 0 1 0
    dz = L.V3 0 0 1

inBoundsV :: L.V3 (Int, Int) -> L.V3 Int -> Bool
inBoundsV bds v = and $ inBounds <$> bds <*> v

inBounds :: (Int, Int) -> Int -> Bool
inBounds (a, b) x = a <= x && x <= b

expand :: (Int, Int) -> (Int, Int)
expand (a, b) = (a-1, b+1)

boundsV :: [L.V3 Int] -> L.V3 (Int, Int)
boundsV vs = bounds <$> sequenceA vs

bounds :: [Int] -> (Int, Int)
bounds ns = (minimum ns, maximum ns)

surface :: CubeSet -> Int
surface cs = 6 * Set.size cs - 2 * touchingPairs cs

touchingPairs :: CubeSet -> Int
touchingPairs cs = length $ filter ((== 1) . uncurry distL0) $ choose2 $ Set.toList cs

distL0 :: L.V3 Int -> L.V3 Int -> Int
distL0 u v = sum $ abs (u - v)

readCubeP :: String -> CubeP
readCubeP str = case splitOn "," str of
  [sx,sy,sz] -> L.V3 (read sx) (read sy) (read sz)
  _ -> error "bad read"

type CubeP = L.V3 Int
type CubeSet = Set.Set CubeP