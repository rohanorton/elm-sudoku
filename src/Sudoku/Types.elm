module Sudoku.Types (..) where

import Matrix exposing (Matrix)


type alias Sudoku =
  Matrix Cell


type alias Cell =
  Maybe Int


type alias Location =
  ( Int, Int )


type alias PossibilitiesMap =
  { rowIndex : Int
  , colIndex : Int
  , possibilities : List Int
  }
