module Main where

import System.Environment

import AdventOfCodeY2022.Day16Part2

main :: IO ()
main = do
  args <- getArgs
  case args of
    "run":_ -> run
    "test":_ -> test
    _ -> putStrLn "Defaulting to run." >> run
