module Day01 exposing (part1, part2)

part1: String -> String
part1 = runSolution 1

part2: String -> String
part2 = runSolution 3

runSolution dropN input =
  let readings = readInput input
  in List.map2 Tuple.pair readings (List.drop dropN readings)
    |> List.filter (\(l, r) -> l < r)
    |> List.length
    |> String.fromInt

readInput s = String.lines s |> List.map (String.toInt >> Maybe.withDefault -1)