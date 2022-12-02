module Main where

import System.Environment

import AdventOfCodeY2022.Day1

main :: IO ()
main = do
  args <- getArgs
  case take 1 args of
    [] -> putStrLn "Error: first command line argument should be test input file path."
    filePath:_ -> runWith filePath
