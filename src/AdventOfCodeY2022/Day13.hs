{-# LANGUAGE TupleSections #-}
module AdventOfCodeY2022.Day13 where

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
dataDir = "data/day13/"

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
  print $ lines
    & splitOn [""]
    & map (isRightOrder . readPck)
    & zip [1..]
    & filter snd
    & map fst
    & sum
  let sortedVals = lines
        & splitOn [""]
        & map readPck
        & concatMap (\(vx, vy) -> [vx, vy])
        & ([div2, div6] <>)
        & List.sortBy cmpVal
  printSample sortedVals
  let (Just i2) = List.elemIndex div2 sortedVals
  let (Just i6) = List.elemIndex div6 sortedVals
  print $ (i2 + 1) * (i6 + 1)
  printSample $ lines
    & filter (not . null)
    & traverse (P.parse packet "packet")
    & either (error . show) id

div2 :: Val
div2 = VL [VL [VI 2]]

div6 :: Val
div6 = VL [VL [VI 6]]

isRightOrder :: Pck -> Bool
isRightOrder (vx, vy) = cmpVal vx vy == LT

cmpVal :: Val -> Val -> Ordering
cmpVal vl vr = case (vl, vr) of
  (VI x, VI y) -> compare x y
  (VL [], VL []) -> EQ
  (VL [], VL (y:ys)) -> LT
  (VL (x:xs), VL []) -> GT
  (VL (x:xs), VL (y:ys)) -> case cmpVal x y of
    EQ -> cmpVal (VL xs) (VL ys)
    LT -> LT
    GT -> GT
  (VI x, VL ys) -> cmpVal (VL [VI x]) (VL ys)
  (VL xs, VI y) -> cmpVal (VL xs) (VL [VI y])

readPck :: [String] -> Pck
readPck strs = case strs of
  l:r:[] ->
    let (lvs, _) = readVals 0 l
        (rvs, _) = readVals 0 r
    in  (head lvs, head rvs)
  _ -> error "wrong pck"

type Pck = (Val, Val)

readVals :: Int -> String -> ([Val], String)
readVals lvl str = case str of
  [] -> ([], [])
  '[':str' ->
    let (vals, str'') = readVals (lvl + 1) str'
    in  first (VL vals :) $ readVals lvl str''
  ',':str' -> readVals lvl str'
  ']':str' -> ([], str')
  _ ->
    let (numStr, str') = span isDigit str
    in  first (VI (read numStr) :) $ readVals lvl str'

data Val
  = VL [Val]
  | VI Int
  deriving (Eq, Ord, Show)

packet :: P.Parsec String u Val
packet =
  valueList <|> valueInt

valueList :: P.Parsec String u Val
valueList =
  VL <$> P.between (P.char '[') (P.char ']') (P.sepBy packet (P.char ','))

valueInt :: P.Parsec String u Val
valueInt =
  VI . read <$> P.many1 P.digit