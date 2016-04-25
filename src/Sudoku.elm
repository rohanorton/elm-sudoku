module Sudoku (..) where

import Html exposing (..)
import Html.Events exposing (onClick)
import Html.Attributes exposing (..)
import String
import Sudoku.Types exposing (..)
import Sudoku.Solver
import Sudoku.Utils


type alias Model =
  { sudoku : Sudoku }


init : Model
init =
  { sudoku = exampleBoard }


{-| For the moment use a sample board
-}
exampleBoard : Sudoku
exampleBoard =
  [ "   26 7 1"
  , "68  7  9 "
  , "19   45  "
  , "82 1   4 "
  , "  46 29  "
  , " 5   3 28"
  , "  93   74"
  , " 4  5  36"
  , "7 3 18   "
  ]
    |> Sudoku.Utils.readSudoku



-- Update


type Action
  = Solve


update : Action -> Model -> Model
update action model =
  case action of
    Solve ->
      { model | sudoku = Sudoku.Solver.solve model.sudoku }



-- View


view : Signal.Address Action -> Model -> Html
view address model =
  let
    rowStrings =
      Sudoku.Utils.display model.sudoku
  in
    div
      []
      [ table
          []
          (List.map (tr [ class "row" ] << buildRow) rowStrings)
      , button [ onClick address Solve ] [ text "Solve" ]
      ]


buildRow : String -> List Html
buildRow row =
  let
    cells =
      String.toList row
  in
    List.map createCell cells


createCell : Char -> Html
createCell n =
  td
    [ class "cell" ]
    [ input
        [ type' "text"
        , maxlength 1
        , value (String.fromChar n)
        ]
        []
    ]
