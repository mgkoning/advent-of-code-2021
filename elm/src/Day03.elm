module Day03 exposing (solve)
import Model exposing (PuzzleResult, setPart1, setPart2, emptyPuzzleResult)
import Bitwise
import List
import Html.Attributes exposing (value)

solve: String -> PuzzleResult
solve s =
  let lines = String.lines s
      bits = List.head lines |> Maybe.withDefault "" |> String.length
      values = List.map fromBits lines
      gamma = calculateGammaRating bits values
      epsilon = useBits bits <| Bitwise.complement gamma
      oxygen = calculateOxygenGeneratorRating bits values
      co2 = calculateCo2ScrubberRating bits values
  in setPart1 (String.fromInt <| gamma * epsilon) <| 
    setPart2 (String.fromInt <| oxygen * co2) emptyPuzzleResult

useBits : Int -> Int -> Int
useBits numBits val = Bitwise.and val <| (Bitwise.shiftLeftBy numBits 1) - 1

calculateGammaRating : Int -> List Int -> Int
calculateGammaRating = calculateRating False (<=)

calculateOxygenGeneratorRating : Int -> List Int -> Int
calculateOxygenGeneratorRating = calculateRating True (<=)

calculateCo2ScrubberRating : Int -> List Int -> Int
calculateCo2ScrubberRating = calculateRating True (>)

calculateRating : Bool -> (Int -> Int -> Bool) -> Int -> List Int -> Int
calculateRating keepMatching use1Fn totalBits v =
  let matchesAtBit bit value = bit == Bitwise.and bit value
      calculateRatingInner atBit result values =
        if atBit < 0 then result
        else case values of
          [x] -> x
          _ ->
            let currentBitAs1 = Bitwise.shiftLeftBy atBit 1
                (ones, zeroes) = List.partition (matchesAtBit currentBitAs1) values
                nextBit = if use1Fn (List.length zeroes) (List.length ones) then currentBitAs1
                          else 0
                remainingValues = if not keepMatching then values
                                  else if nextBit == 0 then zeroes
                                       else ones
            in calculateRatingInner (atBit - 1) (Bitwise.or result nextBit) remainingValues
  in calculateRatingInner (totalBits - 1) 0 v

fromBits : String -> Int
fromBits =
  let readBit c = if c == '1' then 1 else 0
      addBit c val = Bitwise.shiftLeftBy 1 val |> Bitwise.or c
  in String.toList >> List.map readBit >> List.foldl addBit 0 