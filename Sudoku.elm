module Sudoku (..) where

import Html exposing (..)
import Html.Attributes exposing (..)
import String


--------------------------------------------------
--------------------------------------------------
-- Rules of Sudoku
-- 1. 9×9 board, divided into 9 subsections
-- 2. Each square can have a number from 1 to 9
-- 3. Numbers must be unique per row
-- 4. Numbers must be unique per column
-- 5. Numbers must be unique per section
--------------------------------------------------
--------------------------------------------------


type alias Model =
  { board : Board }


init : Model
init =
  { board = exampleBoard }


type alias Board =
  List String


{-| For the moment use a sample board
-}
exampleBoard : Board
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



-- Update


{-| No functionality, just placeholder
-}
type Action
  = NoOp


update : Action -> Model -> Model
update action model =
  model



-- View


view : Signal.Address Action -> Model -> Html
view address model =
  table
    []
    (List.map (tr [ class "row" ] << buildRow) model.board)


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
        , value (toString n)
        ]
        []
    ]
