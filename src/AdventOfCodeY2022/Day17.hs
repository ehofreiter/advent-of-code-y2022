{-# LANGUAGE TupleSections #-}
module AdventOfCodeY2022.Day17 where

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

-- > run

-- $> test

dataDir :: FilePath
dataDir = "data/day17/"

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
  let jets = readJets $ head lines
  --printSample jets
  let jetStream = concat $ repeat $ zip [0..] jets
  -- forM_ blocks $ \b -> do
  --   printBlock b
  --   putStrLn "--"
  putStrLn "--part1"
  -- let (_, (_, _, t2022)) = stepStream List.!! 2022
  -- print $ towerTop t2022 + 1
  --mapM_ putStrLn $ take 20 $ showTower t2022
  putStrLn "--part2"
  let printStep (n, (bs, js, t)) = do
        putStrLn $ "-- STEP " <> show n <> " --"
        printB $ initB t $ head bs
        putStrLn $ "Next 5 dirs: " <> show (take 5 js)
        mapM_ putStrLn $ take 20 $ showTower t
  let stepStream = zip [0..] $ iterate dropBlock (blockStream, jetStream, Set.empty)
  --mapM_ printStep (take 2 $ drop 4 stepStream)
  let restarting (n, (bs, js, t)) = case js of
        (m,_):_ -> m < 4
        [] -> False
  --mapM_ printStep (take 5 $ filter restarting stepStream)
  -- Using the "restarting" filter above we can see that for input.txt, we have:
  --   step 1740: BlockDash, next dir (1,DL), height 2693
  --   step 3470: BlockDash, next dir (1,DL), height 5352
  --   step 5200: BlockDash, next dir (1,DL), height 8011
  --   step 6930: BlockDash, next dir (1,DL), height 10670
  -- So each repetition we do 1730 steps (except the first one is 1740) and the
  -- height increases 2659.  So for n >= 1740, step n will look the same as step
  -- m, where m is
  --   m = (n - 1740) `mod` 1730 + 1740
  -- , and then we can compute the height at n:
  --   height_n = height_m + 2659 * ((n-1740) `div` 1730).
  let start = 1740
      period = 1730
      bigN = 1000000000000 :: Integer
      m = (bigN - start) `mod` period + start
      step@(stepN, (bs, js, t)) = stepStream List.!! fromInteger m
      heightM = towerTop t + 1
      heightBigN = fromIntegral heightM + 2659 * ((bigN - start) `div` period)
  putStrLn $ "m=" <> show m <> ", heightM=" <> show heightM <> ", heightBigN=" <> show heightBigN
  printStep step

readJets :: String -> [Dir]
readJets = map readDir

readDir :: Char -> Dir
readDir c = case c of
  '<' -> DL
  '>' -> DR
  _ -> error "bad dir"

type JetStream = [(Int, Dir)]

data Dir = DL | DR
  deriving (Eq, Ord, Show, Enum, Bounded)

data B = B { bPos :: L.V2 Int, bBlock :: Block, bSize :: L.V2 Int }

printB :: B -> IO ()
printB = mapM_ putStrLn . showB

showB :: B -> [String]
showB b
  =  [ "Block: pos=" <> show (bPos b) <> ", size=" <> show (bSize b) ]
  <> showBlock (bBlock b)

initB :: Tower -> Block -> B
initB t block = B
  { bPos = L.V2 2 (towerTop t + 4)
  , bBlock = block
  , bSize = snd <$> blockSize block
  }

dropBlock :: ([Block], JetStream, Tower) -> ([Block], JetStream, Tower)
dropBlock (blocks, ndirs, t) = case blocks of
  [] -> error "empty block stream"
  block:blocks' -> (blocks', ndirs', t')
    where
      (ndirs', t') = go (initB t block) ndirs
      go b ndirs = case ndirs of
        [] -> error "empty jet stream"
        (_,d):nds ->
          let b' = moveBlockDir t b d
          in  case moveBlockDown t b' of
            Nothing -> (nds, freezeBlock t b')
            Just b'' -> go b'' nds

freezeBlock :: Tower -> B -> Tower
freezeBlock t b = Set.union t $ Set.fromList (bCoords b)

moveBlockDown :: Tower -> B -> Maybe B
moveBlockDown t b = if isBlocked t b' || pos' ^. L._y < 0 then Nothing else Just b'
  where
    pos' = bPos b + L.V2 0 (-1)
    b' = b { bPos = pos' }

moveBlockDir :: Tower -> B -> Dir -> B
moveBlockDir t b dir
  | newPosX < 0 || rightX > 6 || isBlocked t b' =
    b
  | otherwise =
    b'
  where
    b' = b { bPos = newPos }
    oldPos = bPos b
    newPos = L.V2 newPosX (oldPos ^. L._y)
    rightX = newPosX + bSize b ^. L._x -- rightmost coord of the block
    newPosX = dirToX dir + oldPos ^. L._x

isBlocked :: Tower -> B -> Bool
isBlocked t b = any (`Set.member` t) $ bCoords b

bCoords :: B -> [L.V2 Int]
bCoords b = map (bPos b +) $ bBlock b

dirToX :: Dir -> Int
dirToX d = case d of
  DL -> -1
  DR -> 1

printTower :: Tower -> IO ()
printTower = mapM_ putStrLn . showTower

showTower :: Tower -> [String]
showTower t
  =  ["Tower: top=" <> show top <> ", height=" <> show (top + 1)]
  -- <> [show t]
  <> map (\s -> "|" <> s <> "|") towerSpots
  <> ["+-------+"]
  where
    top = towerTop t -- max y coord
    towerSpots =
      if Set.null t
      then ["......."]
      else [[spot (L.V2 x y) | x <- [0..6]] | y <- reverse [0..towerTop t]]
    spot v = if Set.member v t then '#' else '.'

-- empty tower has top coordinate of -1, i.e. just below the bottom empty
-- row which is row x = 0.
towerTop :: Tower -> Int
towerTop = fromMaybe (-1) . Set.lookupMax . Set.map (^. L._y)

type Tower = Set.Set (L.V2 Int)

type Block = [L.V2 Int]

printBlock :: Block -> IO ()
printBlock = mapM_ putStrLn . showBlock

showBlock :: Block -> [String]
showBlock block = [[spot (L.V2 x y) | x <- xs] | y <- ys]
  where
    spot v = if v `elem` block then '#' else '.'
    xs = [xMin..xMax]
    ys = reverse [yMin..yMax]
    L.V2 (xMin, xMax) (yMin, yMax) = blockSize block

blockSize :: Block -> L.V2 (Int, Int)
blockSize block = L.V2 (xMin, xMax) (yMin, yMax)
  where
    xMin = minimum $ map (^. L._x) block
    xMax = maximum $ map (^. L._x) block
    yMin = minimum $ map (^. L._y) block
    yMax = maximum $ map (^. L._y) block

blockStream :: [Block]
blockStream = concat $ repeat blocks

blocks :: [Block]
blocks = [blockDash, blockPlus, blockJ, blockI, blockBox]

blockDash :: Block
blockDash = [L.V2 x 0 | x <- [0..3]]

blockPlus :: Block
blockPlus
  =  [L.V2 x 0 | x <- [1]]
  <> [L.V2 x 1 | x <- [0..2]]
  <> [L.V2 x 2 | x <- [1]]

blockJ :: Block
blockJ
  =  [L.V2 x 0 | x <- [0..2]]
  <> [L.V2 x 1 | x <- [2]]
  <> [L.V2 x 2 | x <- [2]]

blockI :: Block
blockI = [L.V2 0 y | y <- [0..3]]

blockBox :: Block
blockBox = L.V2 <$> [0,1] <*> [0,1]
