#!/usr/bin/env bash

day=$1

if [[ ! ($day =~ [0-9]+ && $day -ge 1) ]]
then
  echo "first argument must be a number > 0"
  exit
fi

srcTemplate="src/AdventOfCodeY2022/NewDay.hs"
srcFile="src/AdventOfCodeY2022/Day$day.hs"

if [[ -f $srcFile ]]
then
  echo "'$srcFile' already exists; skipping creation."
elif [[ ! -f $srcTemplate ]]
then
  echo "'$srcTemplate' does not exist; skipping creation of '$srcFile'."
else
  sed "s/NewDay/Day$day/ ; s/dayX/day$day/" "$srcTemplate" > "$srcFile"
  echo "Created '$srcFile' from '$srcTemplate'."
fi

dataDir="data/day$day"

if [[ -e $dataDir ]]
then
  echo "'$dataDir' already exists; skipping creation."
else
  mkdir "$dataDir"
  echo "Created '$dataDir'."
fi

inputFile="$dataDir/input.txt"
testFile="$dataDir/test.txt"

if [[ -f $inputFile ]]
then
  echo "'$inputFile' already exists; skipping creation."
else
  touch "$inputFile"
  echo "day $day input.txt" > "$inputFile"
  echo "Created '$inputFile'."
fi

if [[ -f $testFile ]]
then
  echo "'$testFile' already exists; skipping creation."
else
  touch "$testFile"
  echo "day $day test.txt" > "$testFile"
  echo "Created '$testFile'."
fi
