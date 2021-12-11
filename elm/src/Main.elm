module Main exposing (main)

import Browser
import Html exposing (Html, button, div, text, h5, textarea, p, a)
import Html.Events exposing (onClick, onInput)
import Html.Attributes exposing (value, class, href)
import Day01
import Day02
import Day03
import Model exposing (..)

main : Program () Model Msg
main = Browser.sandbox { init = init, update = update, view = view }

-- MODEL
type alias Model = { day: Puzzle,
                     input: String,
                     result: PuzzleResult }

init : Model
init = 
  let defaultDay = List.reverse allPuzzles |> List.head |> Maybe.withDefault Day01
  in Model defaultDay "" <| PuzzleResult Nothing Nothing

-- UPDATE
type Msg = SetInput String | SetDay Puzzle | RunPuzzle

update : Msg -> Model -> Model
update msg model =
  case msg of
    SetInput input -> { model | input = input }
    SetDay puzzle -> { model | day = puzzle }
    RunPuzzle -> { model | result = runPuzzle model }

runPuzzle : Model -> PuzzleResult
runPuzzle model = case model.day of
  Day01 -> Day01.solve model.input
  Day02 -> Day02.solve model.input
  Day03 -> Day03.solve model.input

-- VIEW
view : Model -> Html Msg
view model =
  div []
    [ subHeader
    , div [] (List.concatMap (dayButton model) allPuzzles)
    , explanationRow
    , rowDiv
        [ div [ class "one-half column"] [inputArea model]
        , div [ class "one-half column"]
          [ button [ onClick RunPuzzle, class "button-primary" ] [ text "Run it!" ]
          , puzzleResult model.result
          ]
        ]
    , footerRow
    ]

subHeader: Html Msg
subHeader = rowDiv [
  p []
    [ text "Solutions for "
    , a [href "https://adventofcode.com/2021"] [text "Advent of Code 2021"]
    , text " in Elm."
    ]
  ]

explanationRow: Html Msg
explanationRow = rowDiv [ p [] [text "Choose a day to solve, paste the input and run it!"] ]

footerRow : Html Msg
footerRow = rowDiv
  [text "Source at "
  , a [ href "https://github.com/mgkoning/advent-of-code-2021"] [text "Github"]
  ]

rowDiv: List (Html Msg) -> Html Msg
rowDiv inner = div [class "row"] inner

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
