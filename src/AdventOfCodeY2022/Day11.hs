{-# LANGUAGE TupleSections #-}
module AdventOfCodeY2022.Day11 where

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
dataDir = "data/day11/"

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
  -- printSample $ lines
  --   & splitOn [""]
  --   & map readMonkey
  -- mapM_ print $ lines
  --   & splitOn [""]
  --   & map readMonkey
  let mm = lines & splitOn [""] & map readMonkey & map (\m -> (mId m, m)) & IntMap.fromList
  --printSample (Map.toList mm)
  --print $ turn 0 (sInit mm)
  let bigMod = mm & IntMap.elems & map mDiv & product
  putStrLn "part 1"
  print $ product $ take 2 $ reverse $ List.sort $ IntMap.elems $ fst $ doAll mm
  putStrLn "part 2"
  print bigMod
  print $ product $ take 2 $ reverse $ List.sort $ IntMap.elems $ fst $ doAll2 bigMod mm
  --print $ IntMap.elems $ fst $ doAll bigMod mm

doAll :: MM -> S
doAll mm = (List.!! 20) $ List.iterate' doRound (sInit mm)

doAll2 :: Int -> MM -> S
doAll2 bigMod mm = (List.!! 10000) $ List.iterate' (doRound2 bigMod) (sInit mm)

sInit :: MM -> S
sInit mm = (IntMap.empty, mm)

doRound :: S -> S
doRound (ts, mm) = foldl' (flip turn) (ts, mm) (IntMap.keys mm)

doRound2 :: Int -> S -> S
doRound2 bigMod (ts, mm) = foldl' (flip (turn2 bigMod)) (ts, mm) (IntMap.keys mm)

turn :: Int -> S -> S
turn mid (ts, mm) = foldl' (turnItem mid) (ts, mm) (mItems (mm IntMap.! mid))

turn2 :: Int -> Int -> S -> S
turn2 bigMod mid (ts, mm) = foldl' (turnItem2 bigMod mid) (ts, mm) (mItems (mm IntMap.! mid))

turnItem :: Int -> (Throws, MM) -> Int -> (Throws, MM)
turnItem id (ts, mm) i =
  let m = mm IntMap.! id
      i' = doOp (mOp m) i
      i'' = i' `div` 3
      throw = i'' `mod` mDiv m == 0
      throwTo = if throw then mT m else mF m
  in  (IntMap.insertWith (+) id 1 ts, IntMap.adjust dropItem id $ IntMap.adjust (addItem i'') throwTo mm)

turnItem2 :: Int -> Int -> (Throws, MM) -> Int -> (Throws, MM)
turnItem2 bigMod id (ts, mm) i =
  let m = mm IntMap.! id
      i' = doOp (mOp m) i
      i'' = i' `mod` bigMod
      throw = i'' `mod` mDiv m == 0
      throwTo = if throw then mT m else mF m
  in  (IntMap.insertWith (+) id 1 ts, IntMap.adjust dropItem id $ IntMap.adjust (addItem i'') throwTo mm)

dropItem :: Monkey -> Monkey
dropItem m = m { mItems = drop 1 $ mItems m }

addItem :: Int -> Monkey -> Monkey
addItem i m = m { mItems = mItems m <> [i] }

doOp :: (String, String, String) -> Int -> Int
doOp (old, op, other) i =
  let a = case old of
        "old" -> i
        _ -> error "not i"
      f = case op of
        "+" -> (+)
        "*" -> (*)
        _ -> error "not +*"
      b = case other of
        "old" -> i
        x -> read x
  in  f a b

type S = (Throws, MM)

type Throws = IntMap.IntMap Int -- MonkeyId -> inspect count
type MM = IntMap.IntMap Monkey -- Id -> Monkey

data Monkey = Monkey { mId :: Int, mItems :: [Int], mOp :: (String, String, String)
                     , mDiv :: Int, mT :: Int, mF :: Int }
  deriving (Eq, Ord, Show)

readMonkey :: [[Char]] -> Monkey
readMonkey strs = case strs of
  sid:sItems:sop:sdiv:st:sf:_ ->
    let id = sid & drop (length "Monkey ") & takeWhile (/= ':') & read
        items = sItems & drop (length "  Starting items: ") & splitOn ", " & map read
        opa:opb:opc:_ = sop & drop (length "  Operation: new = ") & splitOn " "
        d = sdiv & drop (length "  Test: divisible by ") & read
        t = st & drop (length "    If true: throw to monkey ") & read
        f = sf & drop (length "    If false: throw to monkey ") & read
    in  Monkey id items (opa,opb,opc) d t f
  _ -> error "readM"

