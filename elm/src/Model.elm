module Model exposing (..)

type Puzzle = Day01 | Day02 | Day03
type alias PuzzleResult = { part1: Maybe String, part2: Maybe String}

allPuzzles: List Puzzle
allPuzzles = [Day01, Day02, Day03]

puzzleName: Puzzle -> String
puzzleName puzzle = case puzzle of
  Day01 -> "Day 01"
  Day02 -> "Day 02"
  Day03 -> "Day 03"
