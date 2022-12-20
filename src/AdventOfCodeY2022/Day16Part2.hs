{-# LANGUAGE TupleSections #-}
module AdventOfCodeY2022.Day16Part2 where

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

-- > test

dataDir :: FilePath
dataDir = "data/day16/"

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
  let vs = Map.fromList $ [(vName v, v) | v <- map readValve lines]
  --printSample $ Map.toList vs
  let rvs = getRealValves vs
  --print $ Set.size rvs
  let rvns = Set.map vName rvs
  let startPaths = realPathsFrom vs rvns "AA"
  --printSample startPaths
  -- let startPathCosts = pathCosts vs rvns "AA"
  -- printSample $ Map.toList startPathCosts
  -- printSample $ Map.toList $ pathCosts vs rvns "HR"
  -- printSample $ Map.toList $ pathCosts vs rvns "CN"
  let pcs = allPathCosts vs (Set.insert "AA" rvns)
  --printSample $ Map.toList pcs
  --printSample $ stepRoute vs pcs rvns initRoute
  let searchSteps = iterateM (stepSearch vs pcs rvns) (Right initSearch)
  putStrLn "part2"
  case find isLeft searchSteps of
    Just (Left (Just r)) -> do
      print r
      print $ rPressure r
    _ -> error "didn't find best route"

initSearch :: SS
initSearch = SS
  { ssBestDone = Nothing
  , ssRoutes = Set.singleton initRoute
  }

data SS = SS
  { ssBestDone :: Maybe Route -- finished route with highest pressure
  , ssRoutes :: Set.Set Route -- in-progress routes ordered by pressure
  }

stepSearch :: VS -> PCS -> Set.Set String -> SS -> Either (Maybe Route) SS
stepSearch vs pcs rvns ss0 = case Set.maxView (ssRoutes ss0) of
  Nothing -> Left $ ssBestDone ss0
  Just (bestRoute, otherRoutes) -> case stepRoute vs pcs rvns bestRoute of
    []
      | Just (rPressure bestRoute) > (rPressure <$> ssBestDone ss0) ->
        Right SS
          { ssBestDone = Just bestRoute
          , ssRoutes = pruneRoutes (Just bestRoute) otherRoutes
          }
      | otherwise ->
        Right SS
          { ssBestDone = ssBestDone ss0
          , ssRoutes = otherRoutes
          }
    newRoutes ->
      Right SS
          { ssBestDone = ssBestDone ss0
          , ssRoutes = otherRoutes <> pruneRoutes (ssBestDone ss0) (Set.fromList newRoutes)
          }
  where
    pruneRoutes mRoute rs = case mRoute of
      Nothing -> rs
      Just br -> Set.filter (\r -> rPressure br < maxPressurePossible vs rvns r) rs


initRoute :: Route
initRoute = Route
  { rPressure = 0
  , rOpenedAtMe = [(0, "AA")]
  , rTimeLeftMe = 26
  , rOpenedAtEl = [(0, "AA")]
  , rTimeLeftEl = 26
  }

maxPressurePossible :: VS -> Set.Set String -> Route -> Int
maxPressurePossible vs rvns r = rPressure r + timeLeft * sum remainingFlows
  where
    remainingFlows = map (vFlow . (vs Map.!)) $ Set.toList $ remaining rvns r
    timeLeft = max (rTimeLeftMe r) (rTimeLeftEl r)

remaining :: Set.Set String -> Route -> Set.Set String
remaining rvns r = Set.difference rvns (opened r)

opened :: Route -> Set.Set String
opened r
  =  Set.fromList (map snd $ rOpenedAtMe r)
  <> Set.fromList (map snd $ rOpenedAtEl r)

data Route = Route
  { rPressure :: Int
  , rOpenedAtMe :: [(Int, String)]
  , rTimeLeftMe :: Int
  , rOpenedAtEl :: [(Int, String)]
  , rTimeLeftEl :: Int
  }
  deriving (Eq, Ord, Show)

stepRoute :: VS -> PCS -> Set.Set String -> Route -> [Route]
--stepRoute vs pcs rvns r0 = mapMaybe stepRouteTo [(isMe, v) | v <- nextVs, isMe <- [True, False]]
stepRoute vs pcs rvns r0 = mapMaybe stepRouteTo nextVs
  where
    stepRouteTo v
      | isMe && timeLeftMe <= 0 = Nothing
      | isMe = Just r0
        { rPressure = newPressureMe
        , rOpenedAtMe = (timeLeftMe, vName v) : rOpenedAtMe r0
        , rTimeLeftMe = timeLeftMe
        }
      | not isMe && timeLeftEl <= 0 = Nothing
      | otherwise = Just r0
        { rPressure = newPressureEl
        , rOpenedAtEl = (timeLeftEl, vName v) : rOpenedAtEl r0
        , rTimeLeftEl = timeLeftEl
        }
      where
        isMe = timeLeftMe >= timeLeftEl
        newPressureMe = rPressure r0 + vFlow v * timeLeftMe
        timeLeftMe = rTimeLeftMe r0 - timeCostMe
        timeCostMe = 1 + pcs Map.! Set.fromList [vName v, prevValveMe]
        prevValveMe = snd $ head $ rOpenedAtMe r0
        newPressureEl = rPressure r0 + vFlow v * timeLeftEl
        timeLeftEl = rTimeLeftEl r0 - timeCostEl
        timeCostEl = 1 + pcs Map.! Set.fromList [vName v, prevValveEl]
        prevValveEl = snd $ head $ rOpenedAtEl r0
    nextVs = map (vs Map.!) $ Set.toList $ Set.difference rvns (opened r0)

type PCS = Map.Map (Set.Set String) Int

allPathCosts :: VS -> Set.Set String -> PCS
allPathCosts vs rvns = case Set.minView rvns of
  Nothing -> Map.empty
  Just (vn, rvns') -> pathCosts vs rvns vn <> allPathCosts vs rvns'

pathCosts :: VS -> Set.Set String -> String -> PCS
pathCosts vs rvns vn =
  Map.fromList [(Set.fromList [vn, end], cost) | (cost, end) <- cps]
  where
    cps = realPathsFrom vs rvns vn

realPathsFrom :: VS -> Set.Set String -> String -> [(Int, String)]
realPathsFrom vs rvns vn
  = List.filter ((`Set.member` rvns) . snd)
  $ List.filter ((/= vn) . snd)
  $ pathsFrom vs vn

pathsFrom :: VS -> String -> [(Int, String)]
pathsFrom vs = minCostSearch getNbors
  where
    getNbors vn = map (1 ,) $ vNbors $ vs Map.! vn

hasFlow :: Valve -> Bool
hasFlow = (> 0) . vFlow

getRealValveNames :: VS -> Set.Set String
getRealValveNames = Set.fromList . map vName . List.filter hasFlow . Map.elems

getRealValves :: VS -> Set.Set Valve
getRealValves = Set.fromList . List.filter hasFlow . Map.elems

type VS = Map.Map String Valve

data Valve = Valve
  { vName :: String
  , vFlow :: Int
  , vNbors :: [String]
  }
  deriving (Eq, Ord, Show)

readValve :: String -> Valve
readValve str = Valve { vName = name, vFlow = flow, vNbors = nbors}
  where
    ws = words str
    name = ws List.!! 1
    flow = read $ takeWhile isDigit $ fromJust $ List.stripPrefix "rate=" $ ws List.!! 4
    nbors = map (takeWhile isAlpha) $ drop 9 ws