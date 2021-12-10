module Day01 exposing (solve)
import Model exposing (PuzzleResult)

solve: String -> PuzzleResult
solve i = PuzzleResult (Just <| runSolution 1 i) (Just <| runSolution 3 i)

runSolution : Int -> String -> String
runSolution dropN input =
  let readings = readInput input
  in List.map2 Tuple.pair readings (List.drop dropN readings)
    |> List.filter (\(l, r) -> l < r)
    |> List.length
    |> String.fromInt

readInput : String -> List Int
readInput s = String.lines s |> List.map (String.toInt >> Maybe.withDefault -1)