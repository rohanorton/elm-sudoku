module SudokuSolver (..) where

import Matrix exposing (Matrix)
import Matrix.Extra
import List.Extra
import Array
import String


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


catMaybes : List (Maybe a) -> List a
catMaybes xs =
  case xs of
    [] ->
      []

    Nothing :: xs' ->
      catMaybes xs'

    (Just x) :: xs' ->
      x :: catMaybes xs'


emptyCellPossibilities : Sudoku -> List PossibilitiesMap
emptyCellPossibilities sudoku =
  let
    toPossibles colIndex rowIndex cell =
      case cell of
        Nothing ->
          Just
            { rowIndex = rowIndex
            , colIndex = colIndex
            , possibilities = getPossible sudoku rowIndex colIndex
            }

        -- if there's already something in the list, no need to work out possibilities
        Just a ->
          Nothing
  in
    Matrix.indexedMap toPossibles sudoku
      |> .data
      |> Array.toList
      |> catMaybes
      |> sortCellPossibilities


cols : Sudoku -> List (List Cell)
cols =
  List.Extra.transpose << rows


rows : Sudoku -> List (List Cell)
rows =
  chunk 9 << Array.toList << .data


blocks : Sudoku -> List (List Cell)
blocks =
  let
    pack =
      partition << List.map partition

    unpack =
      List.map List.concat << List.concat

    partition =
      chunk 3
  in
    unpack << List.map List.Extra.transpose << pack << matrixToList


blockIndex : Int -> Int -> Int
blockIndex colIndex rowIndex =
  colIndex // 3 + (rowIndex // 3) * 3


getBlock : Int -> Sudoku -> List Cell
getBlock index sudoku =
  blocks sudoku
    |> flip List.Extra.getAt index
    |> Maybe.withDefault []


getRow : Int -> Sudoku -> List Cell
getRow index sudoku =
  Matrix.getRow index sudoku
    |> Maybe.withDefault Array.empty
    |> Array.toList


noDuplicates : List Cell -> Bool
noDuplicates list =
  case list of
    [] ->
      True

    Nothing :: xs ->
      noDuplicates xs

    x :: xs ->
      not (List.member x xs) && noDuplicates xs


getCol : Int -> Sudoku -> List Cell
getCol index sudoku =
  Matrix.getColumn index sudoku
    |> Maybe.withDefault Array.empty
    |> Array.toList


getPossible : Sudoku -> Int -> Int -> List Int
getPossible sudoku rowIndex colIndex =
  let
    isPossible int =
      List.all (noDuplicates << (::) (Just int)) [ row, col, block ]

    row =
      getRow rowIndex sudoku

    col =
      getCol colIndex sudoku

    block =
      getBlock (blockIndex colIndex rowIndex) sudoku
  in
    List.filter isPossible [1..9]


sortCellPossibilities : List PossibilitiesMap -> List PossibilitiesMap
sortCellPossibilities =
  List.sortBy (List.length << .possibilities)


fillInSingles : Sudoku -> List PossibilitiesMap -> Sudoku
fillInSingles sudoku possMap =
  case possMap of
    [] ->
      sudoku

    x :: xs ->
      case x.possibilities of
        int :: [] ->
          Matrix.set x.colIndex x.rowIndex (Just int) sudoku
            |> flip fillInSingles xs

        _ ->
          fillInSingles sudoku xs


hasPrunable : List PossibilitiesMap -> Bool
hasPrunable possMap =
  case possMap of
    [] ->
      False

    x :: xs ->
      x.possibilities
        |> List.length
        |> (==) 1


prune : Sudoku -> Sudoku
prune sudoku =
  let
    possMap =
      emptyCellPossibilities sudoku
  in
    if (hasPrunable possMap) then
      fillInSingles sudoku possMap
        |> prune
    else
      sudoku


solve : Sudoku -> Sudoku
solve =
  prune



--- converting strings


{-| anything that fails to be parsed to int (i.e. ".", "x", or " ")
    is treated as a Nothing
-}
readCell : String -> Cell
readCell =
  Result.toMaybe << String.toInt


readRow : String -> List Cell
readRow =
  List.map readCell << String.split ""


readSudoku : String -> Sudoku
readSudoku str =
  String.split "\n" str
    |> List.map readRow
    |> Matrix.fromList
    |> Maybe.withDefault Matrix.empty


--------- For Testing --------


exampleBoard : Sudoku
exampleBoard =
  String.trim
    """
36..712..
.5....18.
..92.47..
....13.28
4..5.2..9
27.46....
..53.89..
.83....6.
..769..43
"""
    |> readSudoku


correctBoard : Sudoku
correctBoard =
  String.trim
    """
123456789
456789123
789123456
234567891
567891234
891234567
345678912
678912345
813245678
"""
    |> readSudoku


incorrectBoard : Sudoku
incorrectBoard =
  String.trim
    """
123456789
456789123
789123456
234567891
567891234
891234567
345678912
678912345
813245671
"""
    |> readSudoku
