module Main exposing (main)

import Browser
import Html exposing (Html, button, div, text)
import Html.Events exposing (onClick, on)
import Html exposing (textarea)
import Html.Events exposing (onInput)
import Html exposing (select)
import Html exposing (option)
import Html.Attributes exposing (value)
import Json.Decode as D
import Html.Events exposing (targetValue)
import Html exposing (br)
import Day01 exposing (part1)

main : Program () Model Msg
main = Browser.sandbox { init = init, update = update, view = view }

-- MODEL
type alias Model = { day: Puzzle,
                     input: String,
                     result: PuzzleResult }
type Puzzle = Day01
type alias PuzzleResult = { part1: Maybe String, part2: Maybe String}

puzzleName: Puzzle -> String
puzzleName puzzle = case puzzle of Day01 -> "Day 01"

init : Model
init = Model Day01 "" (PuzzleResult Nothing Nothing)

-- UPDATE
type Msg = SetInput String | SetDay Puzzle | RunPart1 | RunPart2 | RunBoth

update : Msg -> Model -> Model
update msg model =
  case msg of
    SetInput input -> { model | input = input }
    SetDay puzzle -> { model | day = puzzle }
    RunPart1 ->
      { model | result = PuzzleResult (Just <| runPart1 model) Nothing }
    RunPart2 ->
      { model | result = PuzzleResult Nothing (Just <| runPart2 model) }
    RunBoth ->
      { model | result = PuzzleResult (Just <| runPart1 model) (Just <| runPart2 model) }

runPart1 : Model -> String
runPart1 model = case model.day of
  Day01 -> Day01.part1 model.input

runPart2 : Model -> String
runPart2 model = case model.day of
  Day01 -> Day01.part2 model.input

-- VIEW
view : Model -> Html Msg
view model =
  div []
    [ textarea [ onInput SetInput ] [ text model.input ]
    , br [] []
    , select [ on "change" (D.map SetDay puzzleDecoder) ] [ puzzleOption Day01 ]
    , button [ onClick RunPart1 ] [ text "Part 1" ]
    , button [ onClick RunPart2 ] [ text "Part 2" ]
    , button [ onClick RunBoth ] [ text "Both parts" ]
    , br [] []
    , puzzleResult model.result
    ]

puzzleResult: PuzzleResult -> Html Msg
puzzleResult r =
  let partResult title value =
        div [] (case value of Nothing -> []
                              Just v -> [text title, br [] [], text v])
  in div [] [partResult "Part 1" r.part1, partResult "Part 2" r.part2]

puzzleOption: Puzzle -> Html Msg
puzzleOption puzzle = option [ value <| toString puzzle ] [ text <| puzzleName puzzle ]

puzzleDecoder: D.Decoder Puzzle
puzzleDecoder =
  targetValue |> D.andThen (
    \val -> case val of
      "d01" -> D.succeed Day01
      _ -> D.fail ("Invalid puzzle " ++ val))

toString: Puzzle -> String
toString puzzle = case puzzle of Day01 -> "d01"