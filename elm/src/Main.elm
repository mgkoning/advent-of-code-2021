module Main exposing (main)

import Browser
import Html exposing (Html, button, div, text, h5, textarea, p)
import Html.Events exposing (onClick, onInput)
import Html.Attributes exposing (value, class)
import Day01
import Model exposing (..)

main : Program () Model Msg
main = Browser.sandbox { init = init, update = update, view = view }

-- MODEL
type alias Model = { day: Puzzle,
                     input: String,
                     result: PuzzleResult }

init : Model
init = Model Day01 "" (PuzzleResult Nothing Nothing)

-- UPDATE
type Msg = SetInput String | SetDay Puzzle | RunPuzzle

update : Msg -> Model -> Model
update msg model =
  case msg of
    SetInput input -> { model | input = input }
    SetDay puzzle -> { model | day = puzzle }
    RunPuzzle ->
      { model | result = runPuzzle model }

runPuzzle : Model -> PuzzleResult
runPuzzle model = case model.day of
  Day01 -> Day01.solve model.input
  Day02 -> PuzzleResult Nothing Nothing

-- VIEW
view : Model -> Html Msg
view model =
  div []
    [ div [] (List.concatMap (dayButton model) allPuzzles)
    , div [ class "row" ]
        [ div [ class "one-half column"] [inputArea model]
        , div [ class "one-half column"]
          [ button [ onClick RunPuzzle, class "button-primary" ] [ text "Run it!" ]
          , puzzleResult model.result
          ]
        ]
    ]

inputArea: Model -> Html Msg
inputArea model =
  textarea [ onInput SetInput, class "u-full-width puzzle-input" ] [ text model.input ]

dayButton : Model -> Puzzle -> List (Html Msg)
dayButton model puzzle =
  let buttonClass = if puzzle == model.day then "button-primary" else ""
  in [
    button
      [ onClick (SetDay puzzle), class buttonClass ]
      [ text (puzzleName puzzle) ]
    , text " "
  ]


puzzleResult: PuzzleResult -> Html Msg
puzzleResult r =
  let partResult: String -> Maybe String -> List (Html Msg)
      partResult title value =
        case value of Nothing -> []
                      Just v -> [h5 [] [text title], p [] [text v]]
  in div [] (List.concat [partResult "Part 1" r.part1, partResult "Part 2" r.part2])
