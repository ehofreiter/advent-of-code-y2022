#!/usr/bin/env bash

day=$1

if [[ ! ($day =~ [0-9]+ && $day -ge 1 && $day -le 25) ]]
then
  echo "first argument must be a number 1 - 25"
  exit
fi

file="src/AdventOfCodeY2022/Day$day.hs"

if [[ ! -f $file ]]
then
  echo "file '$file' does not exist"
  exit
fi

stack exec ghcid -- --allow-eval "$file"
