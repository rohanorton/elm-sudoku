module Sudoku.Solver (..) where

import Sudoku.Types exposing (..)
import Sudoku.Utils exposing (..)
import Matrix exposing (Matrix)
import List.Extra
import Array


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


isJust : Maybe a -> Bool
isJust x =
  case x of
    Just a ->
      True

    Nothing ->
      False


isComplete : Sudoku -> Bool
isComplete =
  List.all isJust << Array.toList << .data


valid : Sudoku -> Bool
valid sudoku =
  isComplete sudoku
    && List.all noDuplicates (blocks sudoku)
    && List.all noDuplicates (rows sudoku)
    && List.all noDuplicates (cols sudoku)


backtrack : List PossibilitiesMap -> Sudoku -> Sudoku
backtrack possMaps sudoku =
  case possMaps of
    [] ->
      sudoku

    possMap :: possMaps' ->
      let
        attempt colIndex rowIndex possibilities =
          case possibilities of
            [] ->
              sudoku

            possible :: possibilities' ->
              let
                sudoku' =
                  Matrix.set colIndex rowIndex (Just possible) sudoku

                result =
                  solve sudoku'
              in
                if (valid result) then
                  result
                else
                  attempt colIndex rowIndex possibilities'
      in
        attempt possMap.colIndex possMap.rowIndex possMap.possibilities


possMapSudoku : Sudoku -> ( List PossibilitiesMap, Sudoku )
possMapSudoku sudoku =
  ( emptyCellPossibilities sudoku, sudoku )


solve : Sudoku -> Sudoku
solve =
  prune >> possMapSudoku >> uncurry backtrack