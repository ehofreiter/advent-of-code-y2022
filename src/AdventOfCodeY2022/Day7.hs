{-# LANGUAGE TupleSections #-}
module AdventOfCodeY2022.NewDay where

import           Control.Applicative
import           Control.Lens
import           Control.Monad
import           Data.Bifunctor
import           Data.Char
import           Data.Foldable
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

-- $> run

-- $> test

dataDir :: FilePath
dataDir = "data/day7/"

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
  let cmds = map readCmd lines
  printSample cmds
  let fileSys = buildFiles [] cmds
  --mapM_ print $ take 10 $ Map.toList fileSys
  -- let dirS = map (dirSize fileSys) $ Map.keys fileSys
  -- print $ dirS
  --   & filter (<= 100000)
  --   & sum
  let dirS = Map.mapWithKey (\k a -> dirSize fileSys k) fileSys
  let minDirSize = getFreeSpaceNeeded fileSys
  print minDirSize
  print $ dirS
    & Map.filter (>= minDirSize)
    & Map.toList
    & minimumBy (\a b -> compare (snd a) (snd b))

getFreeSpaceNeeded :: FileSys -> Int
getFreeSpaceNeeded fs = freeSpace - (totalSpace - dirSize fs ["/"])

totalSpace = 70000000
freeSpace = 30000000

dirSize :: FileSys -> Path -> Int
dirSize fsys p = sum $ map getSize fs
  where
    fs = fsys Map.! p
    getSize f = case f of
      File _ n -> n
      Dir d -> dirSize fsys (d:p)


isFile :: File -> Bool
isFile f = case f of
  File _ _ -> True
  Dir _ -> False

type Path = [String]
type FileSys = Map.Map Path [File]

data File
  = File String Int
  | Dir String
  deriving (Eq, Ord, Show)

data Cmd
  = Cd String
  | CdUp
  | Ls
  | LsFile File
  deriving (Eq, Ord, Show)

buildFiles :: [String] -> [Cmd] -> FileSys
buildFiles path cmds = case cmds of
  c:cmds' -> case c of
    Cd dir -> buildFiles (dir:path) cmds'
    CdUp -> buildFiles (drop 1 path) cmds'
    Ls -> buildFiles path cmds'
    LsFile fl -> insertFile path fl $ buildFiles path cmds'
  [] -> Map.empty

insertFile :: Path -> File -> FileSys -> FileSys
insertFile path file = Map.insertWith (<>) path [file]

readCmd :: String -> Cmd
readCmd s = case splitOn " " s of
  "$":"cd":"..":_ -> CdUp
  "$":"cd":d:_ -> Cd d
  "$":"ls":_ -> Ls
  _ -> LsFile $ rdFile s

rdFile :: String -> File
rdFile s = case splitOn " " s of
  "dir":d:_ -> Dir d
  size:f:_ -> File f (read size)
  _ -> error "rdFile"
