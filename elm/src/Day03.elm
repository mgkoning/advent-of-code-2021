module Day03 exposing (solve)
import Model exposing (PuzzleResult)
import Bitwise exposing (shiftLeftBy, shiftRightZfBy)
import List exposing (head, sum, length)

solve: String -> PuzzleResult
solve s =
  let lines = String.lines s
      bits = head lines |> Maybe.withDefault "" |> String.length
      bitMask = (shiftLeftBy bits 1) - 1
      values = List.map fromBits lines
      gamma = calculateGamma bits values
      epsilon = Bitwise.and (Bitwise.complement gamma) bitMask
  in PuzzleResult (Just <| String.fromInt <| gamma * epsilon) Nothing

calculateGamma : Int -> List Int -> Int
calculateGamma n values =
  let valueCount = length values
      gammaRateInner s result =
        if n < s then result
        else
          let oneBits = sum <| List.map (shiftRightZfBy s >> Bitwise.and 1) values
              zeroBits = valueCount - oneBits
              nextBit = if zeroBits < oneBits then 1 else 0
          in gammaRateInner (s + 1) (Bitwise.or result (shiftLeftBy s nextBit))
  in gammaRateInner 0 0

fromBits : String -> Int
fromBits =
  let readBit c = if c == '1' then 1 else 0
      addBit c val = shiftLeftBy 1 val |> Bitwise.or c
  in String.toList >> List.map readBit >> List.foldl addBit 0 