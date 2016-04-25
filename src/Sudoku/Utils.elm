module Sudoku.Utils (..) where

import Sudoku.Types exposing (..)
import Matrix exposing (Matrix)
import String
import Array


{-| anything that fails to be parsed to int (i.e. ".", "x", or " ")
    is treated as a Nothing
-}
readCell : String -> Cell
readCell =
  Result.toMaybe << String.toInt


readRow : String -> List Cell
readRow =
  List.map readCell << String.split ""


readSudoku : List String -> Sudoku
readSudoku =
  Maybe.withDefault Matrix.empty << Matrix.fromList << List.map readRow


stringify : Maybe a -> String
stringify maybe =
  case maybe of
    Nothing ->
      " "

    Just a ->
      toString a


matrixToList : Matrix a -> List (List a)
matrixToList matrix =
  let
    -- presume matrix is a square
    ( size, _ ) =
      matrix.size
  in
    Array.toList matrix.data
      |> chunk size


chunk : Int -> List a -> List (List a)
chunk n xs =
  case xs of
    [] ->
      []

    xs ->
      List.take n xs :: chunk n (List.drop n xs)


display : Sudoku -> List String
display sudoku =
  Matrix.map stringify sudoku
    |> matrixToList
    |> List.map (List.foldl (++) "")
