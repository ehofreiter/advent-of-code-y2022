{-# LANGUAGE TupleSections #-}
module AdventOfCodeY2022.Day9 where

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
dataDir = "data/day9/"

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
  --printSample lines
  let moves = map readMove lines
  printSample moves
  -- let (rope, tailPs) = doMoves $ concatMap moveDirs moves
  -- print $ Set.size tailPs
  let (rope, tailPs) = doMoves' $ concatMap moveDirs moves
  print $ Set.size tailPs


  -- print $ moveTail (Rope (L.V2 1 (-2)) (L.V2 1 (-2)))
  -- print $ (L.V2 1 (-2)) - (L.V2 1 (-2))
  --mapM_ print $ Set.toList tailPs
  -- mapM_ (\(d, (r,ts)) -> print (d,r)) $ zip tstin $ drop 1 $ List.scanl' (flip doMove) (Rope 0 0, Set.empty) tstin
  --   where
  --     tstin = [R,R,R,R,U,U,U,U,L,L,L,D,R,R,R,R,D,L,L,L,L,L,R,R]

type Ropes = [P]

doMove' :: Dir -> (Ropes, Set.Set P) -> (Ropes, Set.Set P)
doMove' d (ropes, tailPs) = (ropes', tailPs')
  where
    (ropes', tl) = moveRopes d ropes
    tailPs' = Set.insert tl tailPs

doMoves' :: [Dir] -> (Ropes, Set.Set P)
doMoves' ms = loop ms (replicate 10 0) (Set.singleton 0)
  where
    loop ms rope tailPs = case ms of
      [] -> (rope, tailPs)
      m:ms' -> loop ms' rope' tailPs'
        where
          (rope', tailPs') = doMove' m (rope, tailPs)

moveRopes :: Dir -> Ropes -> (Ropes, P)
moveRopes dir ropes = case ropes of
  [] -> error "no ropes"
  hrope:ropes' ->
    let result = loop (dirToV2 dir + hrope) ropes'
    in  (result, last result)
  where
    loop p rs = case rs of
      r:rs' -> p : loop (moveTail (Rope p r)) rs'
      [] -> [p]

moveDirs :: Move -> [Dir]
moveDirs (Move d i) = replicate i d

doMove :: Dir -> (Rope, Set.Set P) -> (Rope, Set.Set P)
doMove d (rope, tailPs) = (rope', tailPs')
  where
    rope' = moveRope d rope
    tailPs' = Set.insert tl tailPs
    Rope _hd tl = rope'

doMoves :: [Dir] -> (Rope, Set.Set P)
doMoves ms = loop ms (Rope 0 0) (Set.singleton 0)
  where
    loop ms rope tailPs = case ms of
      [] -> (rope, tailPs)
      m:ms' -> loop ms' rope' tailPs'
        where
          (rope', tailPs') = doMove m (rope, tailPs)

type P = L.V2 Int
data Rope = Rope P P -- head tail
  deriving (Eq, Ord, Show)

moveRope :: Dir -> Rope -> Rope
moveRope dir (Rope h t) = Rope h' t'
  where
    h' = dirToV2 dir + h
    t' = moveTail (Rope h' t)

moveTail :: Rope -> P
moveTail (Rope h t) = case h - t of
  L.V2 0 y
    | y > 1 -> t + L.V2 0 1
    | y < -1 -> t - L.V2 0 1
    | otherwise -> t
  L.V2 x 0
    | x > 1 -> t + L.V2 1 0
    | x < -1 -> t - L.V2 1 0
    | otherwise -> t
  L.V2 x y
    | abs x > 1 || abs y > 1 -> t + L.V2 (signum x) (signum y)
    | otherwise -> t

readMove :: String -> Move
readMove s = case splitOn " " s of
  d:nstr:_ -> Move (readDir d) (read nstr)
  _ -> error "bad move"

readDir :: String -> Dir
readDir s = case s of
  "U" -> U
  "D" -> D
  "L" -> L
  "R" -> R
  _ -> error "bad dir"

data Dir = U | D | L | R
  deriving (Eq, Ord, Show, Enum, Bounded)

data Move = Move Dir Int
  deriving (Eq, Ord, Show)

moveToV2 :: Move -> L.V2 Int
moveToV2 (Move d i) = case d of
  U -> L.V2 0 (-i)
  D -> L.V2 0 i
  L -> L.V2 (-i) 0
  R -> L.V2 i 0

dirToV2 :: Dir -> L.V2 Int
dirToV2 d = case d of
  U -> L.V2 0 (-1)
  D -> L.V2 0 1
  L -> L.V2 (-1) 0
  R -> L.V2 1 0
