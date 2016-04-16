module SudokuSolver (..) where

import String


type Sudoku
  = Sudoku (List Row)


type alias Row =
  List (Cell)


type alias Cell =
  Maybe Int


type alias Position =
  ( Int, Int )


rows : Sudoku -> List Row
rows sudoku =
  case sudoku of
    Sudoku rs ->
      rs


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


noDuplicates : List a -> Bool
noDuplicates list =
  case list of
    [] ->
      True

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
  readSudoku
    """36..712..
.5....18.
..92.47..
....13.28
4..5.2..9
27.46....
..53.89..
.83....6.
..769..43"""


correctBoard : Sudoku
correctBoard =
  readSudoku
    """123456789
456789123
789123456
234567891
567891234
891234567
345678912
678912345
813245678"""


incorrectBoard : Sudoku
incorrectBoard =
  readSudoku
    """123456789
456789123
789123456
234567891
567891234
891234567
345678912
678912345
813245671"""
