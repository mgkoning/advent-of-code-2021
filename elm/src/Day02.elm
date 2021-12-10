module Day02 exposing (solve)
import Model exposing (PuzzleResult)

type Instruction = Forward Int | Down Int | Up Int
type alias Position = { hor: Int, depth: Int }
zeroPosition = Position 0 0

type alias AimPos = { aim: Int, pos: Position }
zeroAimPos = AimPos 0 zeroPosition

solve: String -> PuzzleResult
solve s =
  let instructions = readInstructions s
      updatePosition instr pos =
        case instr of
          Forward f -> { pos | hor = pos.hor + f }
          Down d -> { pos | depth = pos.depth + d }
          Up u -> { pos | depth = pos.depth - u }
      updateAimPos instr aimPos =
        case instr of
          Down d -> { aimPos | aim = aimPos.aim + d }
          Up u -> { aimPos | aim = aimPos.aim - u }
          Forward f ->
            let oldPos = aimPos.pos
                newPos = { oldPos | hor = oldPos.hor + f
                                  , depth = oldPos.depth + f * aimPos.aim }
            in { aimPos | pos = newPos }
      part1 = let finalPos = List.foldl updatePosition zeroPosition instructions
              in finalPos.hor * finalPos.depth
      part2 = let finalPos = List.foldl updateAimPos zeroAimPos instructions
              in finalPos.pos.hor * finalPos.pos.depth
  in PuzzleResult (Just <| String.fromInt part1) (Just <| String.fromInt part2)

readInstructions: String -> List Instruction
readInstructions = 
  let readInstruction s =
        case String.split " " s of
              [i, v] ->
                let value = String.toInt v |> Maybe.withDefault -1
                in case i of "forward" -> Just <| Forward value
                             "down" -> Just <| Down value
                             _ -> Just <| Up value
              _ -> Nothing
  in String.lines >> List.map readInstruction >> List.concatMap maybeToList

maybeToList : Maybe a -> List a
maybeToList m = case m of Just x -> [x]
                          Nothing -> []