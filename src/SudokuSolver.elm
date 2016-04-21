module SudokuSolver (..) where

import String
import List.Extra


type Sudoku
  = Sudoku (List Row)


type alias Row =
  List Cell


type alias Column =
  List Cell


type alias Block =
  List Cell


type alias Cell =
  Maybe Int


type alias Position =
  ( Int, Int )


rows : Sudoku -> List Row
rows sudoku =
  case sudoku of
    Sudoku rs ->
      rs


columns : Sudoku -> List Column
columns =
  List.Extra.transpose << rows


blocks : Sudoku -> List Block
blocks =
  let
    pack =
      partition << List.map partition

    unpack =
      List.map List.concat << List.concat

    partition =
      chunk 3
  in
    unpack << List.map List.Extra.transpose << pack << rows


chunk : Int -> List a -> List (List a)
chunk n xs =
  case xs of
    [] ->
      []

    xs ->
      List.take n xs :: chunk n (List.drop n xs)


isFull : Sudoku -> Bool
isFull =
  List.all isFullRow << rows


isFullRow : List Cell -> Bool
isFullRow =
  List.all isJust


isJust : Maybe a -> Bool
isJust x =
  case x of
    Nothing ->
      False

    Just a ->
      True


isNothing : Maybe a -> Bool
isNothing =
  not << isJust


noDuplicates : List Cell -> Bool
noDuplicates list =
  case list of
    [] ->
      True

    Nothing :: xs ->
      noDuplicates xs

    x :: xs ->
      not (List.member x xs) && noDuplicates xs


{-| checks ALL rows, columns and blocks for duplicates
-}
isValid : Sudoku -> Bool
isValid sudoku =
  List.all noDuplicates (rows sudoku)
    && List.all noDuplicates (columns sudoku)
    && List.all noDuplicates (blocks sudoku)


getRowByPos : Sudoku -> Position -> Maybe Row
getRowByPos sudoku =
  List.Extra.getAt (rows sudoku) << fst


getColumnByPos : Sudoku -> Position -> Maybe Column
getColumnByPos sudoku =
  List.Extra.getAt (columns sudoku) << snd


getBlockByPos : Sudoku -> Position -> Maybe Block
getBlockByPos sudoku =
  List.Extra.getAt (blocks sudoku) << blockIndex


getCell : Sudoku -> Position -> Cell
getCell sudoku ( x, y ) =
  let
    row =
      Maybe.withDefault [] (List.Extra.getAt (rows sudoku) y)
  in
    Maybe.withDefault Nothing (List.Extra.getAt (row) x)


blockIndex : Position -> Int
blockIndex ( x, y ) =
  x // 3 + (y // 3) * 3


type alias PossibilitiesMap =
  { position : Position
  , possibilities : List Int
  }


emptyCellPossibilities : Sudoku -> List PossibilitiesMap
emptyCellPossibilities sudoku =
  let
    getEmpty y =
      List.indexedMap (\x _ -> { position = ( x, y ), possibilities = (getPossible sudoku ( x, y )) })
  in
    rows sudoku
      |> List.indexedMap getEmpty
      |> List.concat
      |> List.filter (not << List.isEmpty << .possibilities)


getPossible : Sudoku -> Position -> List Int
getPossible sudoku pos =
  let
    isPossible n =
      posEmpty && List.all (noDuplicates << (::) (Just n)) [ col, row, block ]

    posEmpty =
      isNothing (getCell sudoku pos)

    col =
      Maybe.withDefault [] (getColumnByPos sudoku pos)

    row =
      Maybe.withDefault [] (getRowByPos sudoku pos)

    block =
      Maybe.withDefault [] (getBlockByPos sudoku pos)
  in
    List.filter isPossible [1..9]


{-| anything that fails to be parsed to int (i.e. ".", "x", or " ")
    is treated as a Nothing
-}
readCell : String -> Cell
readCell =
  Result.toMaybe << String.toInt


readRow : String -> Row
readRow =
  List.map readCell << String.split ""


readSudoku : String -> Sudoku
readSudoku =
  Sudoku << List.map readRow << String.split "\n"



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
