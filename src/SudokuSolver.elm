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


isFull : Sudoku -> Bool
isFull =
  List.all isFullRow << rows


isFullRow : List Cell -> Bool
isFullRow =
  List.all isSomething


isSomething : Maybe a -> Bool
isSomething x =
  case x of
    Nothing ->
      False

    Just a ->
      True


noDuplicates : List Cell -> Bool
noDuplicates list =
  case list of
    [] ->
      True

    Nothing :: xs ->
      noDuplicates xs

    x :: xs ->
      not (List.member x xs) && noDuplicates xs


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
