{-# LANGUAGE TupleSections #-}
module AdventOfCodeY2022.Day15 where

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

-- > test

dataDir :: FilePath
dataDir = "data/day15/"

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
  let ss = map readSensor lines
  -- printSample ss
  -- printSample $ ss
  --   & map sensorDist
  --let rowY = 10 -- test
  let rowY = 2000000 -- run
  let ranges = ss
        & map (\s -> (s,s))
        & map (second (sensedRangeAtY rowY))
        & mapMaybe snd
        & djuRanges
  -- mapM_ print ranges
  putStrLn "part1"
  print $ sum $ map rangeSize $ Set.toList ranges
  putStrLn "part2"
  let yRange = 2*rowY
  let circles = map sensorCircle ss
  let pts
        = Set.fromList
        $ concatMap lineToPts
        $ fst
        $ List.foldl' (addCircle yRange) ([], []) circles
  case Set.toList pts of
    [L.V2 x y] -> print $ x * 4000000 + y
    _ -> error "didn't get unique pt"

addCircle :: Int -> ([Line], [Circle]) -> Circle -> ([Line], [Circle])
addCircle maxCd (lines, circles) (c, r) = (lines', circles')
  where
    lines' = newLines <> oldLines
    newLines = outerLines (c, r)
      & mapMaybe (lineXSquare maxCd)
      & flip linesDCircles circles
    oldLines = linesDCircles lines [(c, r)]
    circles' = (c, r) : circles

linesDCircles :: [Line] -> [Circle] -> [Line]
linesDCircles ls cs = foldr f ls cs
  where
    f c ls = concatMap (flip lineDCircle c) ls

-- could give one line, no lines, or two lines (if circle splits line in two)
-- this is the line minus the circle (using pointwise set difference)
lineDCircle :: Line -> Circle -> [Line]
lineDCircle l@(lp0, lr) c@(cp, cr)
  -- Line is just one point lp0
  | lr == 0 = if lp0 `inCircle` c then [] else [l]
  -- Line is going up-right (y decreases as x increases)
  | lr < 0 =
    if dp0x + dp0y < -cr || dp0x + dp0y > cr
    -- If lp0 is up-left or down-right of the circle, doesn't intersect, just return [l]
    then [l]
    else
      let dropR = (dp1x - dp1y + cr) `div` 2 + 1
          dropL = (-dp0x + dp0y + cr) `div` 2 + 1
      in  catMaybes [dropRight dropR l, dropLeft dropL l]
  -- Line is going down-right (y increases as x increases)
  | otherwise = -- lr > 0
    if dp0x - dp0y < -cr || dp0x - dp0y > cr
    -- If lp0 is down-left or up-right of the circle, doesn't intersect, just return [l]
    then [l] -- lp is down-left of cLeft and going down-right, so l doesn't intersect the circle at all
    else
      let dropR = (dp1x + dp1y + cr) `div` 2 + 1
          dropL = (-dp0x - dp0y + cr) `div` 2 + 1
      in  catMaybes [dropRight dropR l, dropLeft dropL l]
  where
    L.V2 dp0x dp0y = lp0 - cp
    L.V2 dp1x dp1y = lp1 - cp
    lp1 = lp0 + L.V2 (abs lr) lr

dropLeft :: Int -> Line -> Maybe Line
dropLeft n l@(L.V2 lx0 ly0, r)
  | n <= 0 = Just l
  | n > abs r = Nothing
  | otherwise = Just (L.V2 lx0' ly0', r')
    where
      lx0' = lx0 + n
      ly0' = ly0 + n*signum r
      r' = (abs r - n) * signum r

dropRight :: Int -> Line -> Maybe Line
dropRight n l@(L.V2 lx0 ly0, r)
  | n <= 0 = Just l
  | n > abs r = Nothing
  | otherwise = Just (L.V2 lx0 ly0, r')
    where
      r' = (abs r - n) * signum r

-- Intersection of line with square from (0,0) to (n,n)
lineXSquare :: Int -> Line -> Maybe Line
lineXSquare n l
  =   pure l
  >>= rightL 0
  >>= leftL n
  >>= belowL 0
  >>= aboveL n

rightL :: Int -> Line -> Maybe Line
rightL minX l@(L.V2 lx0 _, _) = dropLeft (minX - lx0) l

leftL :: Int -> Line -> Maybe Line
leftL maxX l@(L.V2 lx0 _, r) = dropRight (lx1 - maxX) l
  where
    lx1 = lx0 + abs r

belowL :: Int -> Line -> Maybe Line
belowL minY l@(L.V2 lx0 ly0, r)
  | r >= 0 = dropLeft (minY - ly0) l
  | otherwise = dropRight (minY - ly1) l
    where
      ly1 = ly0 + r

aboveL :: Int -> Line -> Maybe Line
aboveL maxY l@(L.V2 lx0 ly0, r)
  | r <= 0 = dropLeft (ly0 - maxY) l
  | otherwise = dropRight (ly1 - maxY) l
    where
      ly1 = ly0 + r

inCircle :: Cd -> Circle -> Bool
inCircle p (c, r) = distL0 p c <= r

type Circle = (L.V2 Int, Int)

sensorCircle :: Sensor -> Circle
sensorCircle (sc, bc) = (sc, sensorDist (sc, bc))

type Line = (L.V2 Int, Int)

lineToPts :: Line -> [Cd]
lineToPts (pt, r) =
  [ pt + L.V2 d (d*signum r) | d <- [0..abs r] ]

outerLines :: Circle -> [Line]
outerLines (L.V2 cx cy, r)
  | r < 0 = error "negative radius"
  | otherwise =
    let topLeft  = (L.V2 (cx-r-1) cy,      -r)
        botLeft  = (L.V2 (cx-r)   (cy+1),   r)
        topRight = (L.V2 cx       (cy-r-1), r)
        botRight = (L.V2 (cx+1)   (cy+r),  -r)
    in  [topLeft, botLeft, topRight, botRight]

outerPts :: Circle -> Set.Set Cd
outerPts (c, r) = perimeterPts (c, r+1)

perimeterPts :: Circle -> Set.Set Cd
perimeterPts (L.V2 cx cy, r)
  | r < 0 = error "negative radius"
  | otherwise =
    let bottom = Set.fromList [L.V2 (cx + rx) (cy + (r-abs rx)) | rx <- [-r..r-1]]
        top    = Set.fromList [L.V2 (cx + rx) (cy - (r-abs rx)) | rx <- [-r+1..r]]
    in  Set.union bottom top

rangesAtY :: Int -> [Sensor] -> Set.Set Range
rangesAtY y ss = ss
  & map (\s -> (s,s))
  & map (second (sensedRangeAtY y))
  & mapMaybe snd
  & djuRanges

djus :: Set.Set Range -> Range -> Set.Set Range
djus rs r = Set.insert overlapR disjoints
  where
    disjoints = Set.filter (disjoint r) rs
    overlapR = Set.foldl' tryMerge r rs
  -- find all intersections with r
  -- if none, insert r
  -- else merge all

disjoint :: Range -> Range -> Bool
disjoint (minA, maxA) (minB, maxB) = maxL+1 < minR
  where
    minL = min minA minB
    minR = max minA minB
    maxL = min maxA maxB
    maxR = max maxA maxB

tryMerge :: Range -> Range -> Range
tryMerge base@(minBase, maxBase) new@(minNew, maxNew) =
  if disjoint base new
  then base
  else (minL, maxR)
  where
    minL = min minBase minNew
    maxR = max maxBase maxNew

-- dju :: Range -> Range -> Set.Set Range
-- dju (minA, maxA) (minB, maxB) =
--   if maxL < minR
--   then Set.fromList [(minL, maxL), (minR, maxR)]
--   else Set.fromList [(minL, maxR)]
--   where
--     minL = min minA minB
--     minR = max minA minB
--     maxL = min maxA maxB
--     maxR = max maxA maxB

type Range = (Int, Int)

rangeSize :: Range -> Int
rangeSize (a, b) = max 0 (b - a)

mRangeSize :: Maybe Range -> Int
mRangeSize = maybe 0 rangeSize

djuSize :: Set.Set Range -> Int
djuSize = sum . map rangeSize . Set.toList

-- disjoint union of ranges
djuRanges :: [Range] -> Set.Set Range
djuRanges rs = case rs of
  [] -> Set.empty
  r:rs' -> foldl' djus (djuOne r) rs'

djuOne :: Range -> Set.Set Range
djuOne (minA, maxA)
  | minA <= maxA = Set.singleton (minA, maxA)
  | otherwise = Set.empty

sensedRangesAtY :: Int -> [Sensor] -> [Maybe Range]
sensedRangesAtY y = map (sensedRangeAtY y)

sensedRangeAtY :: Int -> Sensor -> Maybe Range
sensedRangeAtY y s@(L.V2 scx scy, _)
  | d < dy = Nothing
  | otherwise = Just (scx - dx, scx + dx)
  where
    d = sensorDist s
    dy = abs $ y - scy
    dx = d - dy

sensorCds :: [Sensor] -> Map.Map Cd Int
sensorCds = Map.fromList . map (\(s, b) -> (s, sensorDist (s, b)))

beaconCds :: [Sensor] -> Set.Set Cd
beaconCds = Set.fromList . map snd

sensorDist :: Sensor -> Int
sensorDist (sc, bc) = distL0 sc bc

distL0 :: L.V2 Int -> L.V2 Int -> Int
distL0 (L.V2 sx sy) (L.V2 bx by) = abs (sx - bx) + abs (sy - by)

type Cd = L.V2 Int

type Sensor = (Cd, Cd) -- sensor pos, beacon pos

readSensor :: String -> Sensor
readSensor str =
  let [s, b] = splitOn ": " str
      ["Sensor", "at", xstr, ystr] = splitOn " " s
      ["closest", "beacon", "is", "at", bxstr, bystr] = splitOn " " b
  in (readCd xstr ystr, readCd bxstr bystr)

readCd :: String -> String -> Cd
readCd xstr ystr =
  let x = read $ takeWhile (/= ',') $ drop 2 xstr
      y = read $ drop 2 ystr
  in  L.V2 x y