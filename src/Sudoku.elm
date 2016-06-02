module Sudoku exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, on, targetValue)
import Matrix exposing (Matrix)
import Json.Decode as Json
import Array
import List
import List.Extra
import String


-- Model


type alias Model =
    { sudoku : Sudoku
    }


type alias Sudoku =
    Matrix Cell


type Cell
    = Defined Int
    | GoodGuess Int
    | BadGuess Int
    | Empty


init : ( Model, Cmd Msg )
init =
    { sudoku = exampleBoard
    }
        ! []


type RowIndex
    = RowIndex Int


type ColIndex
    = ColIndex Int


type BlockIndex
    = BlockIndex Int


type alias PossibilitiesMap =
    { rowIndex : RowIndex
    , colIndex : ColIndex
    , possibilities : List Int
    }



-- Read


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
        |> readSudoku


readSudoku : List String -> Sudoku
readSudoku =
    Maybe.withDefault Matrix.empty << Matrix.fromList << List.map readRow


readRow : String -> List Cell
readRow =
    List.map readCell << String.split ""


readCell : String -> Cell
readCell str =
    case String.toInt str of
        Ok n ->
            Defined n

        Err err ->
            Empty



-- Update


type Msg
    = New
    | Solve
    | SetSquare ColIndex RowIndex String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg ({ sudoku } as model) =
    case msg of
        New ->
            model ! []

        Solve ->
            let
                sudoku' =
                    solve sudoku
            in
                { model | sudoku = sudoku' } ! []

        SetSquare colIndex rowIndex str ->
            let
                sudoku' =
                    guess colIndex rowIndex str sudoku
            in
                { model | sudoku = sudoku' } ! []



-- Sudoku Utils


solve : Sudoku -> Sudoku
solve =
    uncurry backtrack << possMapSudoku << prune


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


isComplete : Sudoku -> Bool
isComplete =
    List.all isFull << Array.toList << .data


isFull : Cell -> Bool
isFull x =
    not (x == Empty)


valid : Sudoku -> Bool
valid sudoku =
    isComplete sudoku
        && foo (blocks sudoku)
        && foo (rows sudoku)
        && foo (cols sudoku)


foo : List (List Cell) -> Bool
foo =
    List.all noDuplicates << List.map cellInts


cellInts : List Cell -> List Int
cellInts list =
    case list of
        [] ->
            []

        Empty :: xs ->
            cellInts xs

        (GoodGuess x) :: xs ->
            x :: cellInts xs

        (BadGuess x) :: xs ->
            x :: cellInts xs

        (Defined x) :: xs ->
            x :: cellInts xs


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
                                    sudokuSet colIndex rowIndex (GoodGuess possible) sudoku

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


fillInSingles : Sudoku -> List PossibilitiesMap -> Sudoku
fillInSingles sudoku possMap =
    case possMap of
        [] ->
            sudoku

        x :: xs ->
            case x.possibilities of
                int :: [] ->
                    sudokuSet x.colIndex x.rowIndex (GoodGuess int) sudoku
                        |> flip fillInSingles xs

                _ ->
                    fillInSingles sudoku xs


emptyCellPossibilities : Sudoku -> List PossibilitiesMap
emptyCellPossibilities sudoku =
    let
        toPossibles y x cell =
            case cell of
                Empty ->
                    Just
                        { rowIndex = RowIndex x
                        , colIndex = ColIndex y
                        , possibilities = getPossible (ColIndex y) (RowIndex x) sudoku
                        }

                -- if there's already something in the list, no need to work out possibilities
                _ ->
                    Nothing
    in
        Matrix.indexedMap toPossibles sudoku
            |> .data
            |> Array.toList
            |> catMaybe
            |> sortCellPossibilities


sortCellPossibilities : List PossibilitiesMap -> List PossibilitiesMap
sortCellPossibilities =
    List.sortBy (List.length << .possibilities)


catMaybe : List (Maybe a) -> List a
catMaybe xs =
    case xs of
        [] ->
            []

        Nothing :: xs' ->
            catMaybe xs'

        (Just x) :: xs' ->
            x :: catMaybe xs'


hasPrunable : List PossibilitiesMap -> Bool
hasPrunable possMap =
    case possMap of
        [] ->
            False

        x :: xs ->
            x.possibilities
                |> List.length
                |> (==) 1


getPossible : ColIndex -> RowIndex -> Sudoku -> List Int
getPossible colIndex rowIndex sudoku =
    List.filter (isPossible colIndex rowIndex sudoku) [1..9]


guess : ColIndex -> RowIndex -> String -> Sudoku -> Sudoku
guess colIndex rowIndex str sudoku =
    let
        cell =
            getCell colIndex rowIndex str sudoku
    in
        sudokuSet colIndex rowIndex cell sudoku


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


sudokuSet : ColIndex -> RowIndex -> Cell -> Sudoku -> Sudoku
sudokuSet (ColIndex y) (RowIndex x) cell sudoku =
    Matrix.set y x cell sudoku


getCell : ColIndex -> RowIndex -> String -> Sudoku -> Cell
getCell colIndex rowIndex str sudoku =
    let
        int =
            String.toInt str
                |> Result.toMaybe
                |> Maybe.withDefault 0
    in
        if int == 0 then
            Empty
        else if isPossible colIndex rowIndex sudoku int then
            GoodGuess int
        else
            BadGuess int


cols : Sudoku -> List (List Cell)
cols =
    List.Extra.transpose << rows


rows : Sudoku -> List (List Cell)
rows =
    chunk 9 << Array.toList << .data


unpack : List (List (List (List a))) -> List (List a)
unpack =
    List.map List.concat << List.concat


pack : List (List a) -> List (List (List (List a)))
pack =
    partition << List.map partition


partition : List a -> List (List a)
partition =
    chunk 3


blocks : Sudoku -> List (List Cell)
blocks =
    unpack << List.map List.Extra.transpose << pack << matrixToList


getCol : ColIndex -> Sudoku -> List Cell
getCol (ColIndex i) sudoku =
    Matrix.getColumn i sudoku
        |> Maybe.withDefault Array.empty
        |> Array.toList


blockIndex : ColIndex -> RowIndex -> BlockIndex
blockIndex (ColIndex c) (RowIndex r) =
    BlockIndex (c // 3 + (r // 3) * 3)


getBlock : BlockIndex -> Sudoku -> List Cell
getBlock (BlockIndex i) sudoku =
    blocks sudoku
        |> List.Extra.getAt i
        |> Maybe.withDefault []


getRow : RowIndex -> Sudoku -> List Cell
getRow (RowIndex i) sudoku =
    Matrix.getRow i sudoku
        |> Maybe.withDefault Array.empty
        |> Array.toList


cellsToInts : List Cell -> List Int
cellsToInts =
    List.foldl
        (\cell memo ->
            case cell of
                Empty ->
                    memo

                Defined n ->
                    n :: memo

                GoodGuess n ->
                    n :: memo

                BadGuess n ->
                    n :: memo
        )
        []


noDuplicates : List Int -> Bool
noDuplicates list =
    case list of
        [] ->
            True

        x :: xs ->
            not (List.member x xs) && noDuplicates xs


isPossible : ColIndex -> RowIndex -> Sudoku -> Int -> Bool
isPossible colIndex rowIndex sudoku int =
    let
        col =
            getCol colIndex sudoku

        row =
            getRow rowIndex sudoku

        block =
            getBlock (blockIndex colIndex rowIndex) sudoku
    in
        List.map cellsToInts [ row, col, block ]
            |> List.all (noDuplicates << (::) int)



-- Subscriptions


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- View


view : Model -> Html Msg
view model =
    div [ class "container" ]
        [ sudokuView model.sudoku
        , button [ onClick New ] [ text "New" ]
        , button [ onClick Solve ] [ text "Solve" ]
        ]


sudokuView : Sudoku -> Html Msg
sudokuView sudoku =
    let
        rows =
            sudokuToRows sudoku
    in
        table []
            <| (colFormatting ++ (List.map (tbody []) <| chunk 3 rows))


colFormatting : List (Html msg)
colFormatting =
    List.repeat 3 threeColGroup


threeColGroup : Html msg
threeColGroup =
    colgroup [] <| List.repeat 3 <| col [] []


sudokuToRows : Sudoku -> List (Html Msg)
sudokuToRows =
    List.indexedMap rowView << matrixToList


rowView : Int -> List Cell -> Html Msg
rowView y row =
    tr [ class "row" ] <| List.indexedMap (cellView y) row


cellView : Int -> Int -> Cell -> Html Msg
cellView y x cell =
    let
        colIndex =
            ColIndex x

        rowIndex =
            RowIndex y
    in
        td [ class <| "cell " ++ cellClass cell ]
            [ input
                [ type' "text"
                , maxlength 1
                , value <| cellContentString cell
                , readonly <| isReadOnly cell
                , on "input" (Json.map (SetSquare colIndex rowIndex) targetValue)
                ]
                []
            ]


cellClass : Cell -> String
cellClass cell =
    case cell of
        Defined n ->
            "defined"

        GoodGuess n ->
            "good"

        BadGuess n ->
            "bad"

        Empty ->
            ""


isReadOnly : Cell -> Bool
isReadOnly cell =
    case cell of
        Defined n ->
            True

        _ ->
            False


cellContentString : Cell -> String
cellContentString cell =
    case cell of
        Empty ->
            ""

        Defined n ->
            toString n

        GoodGuess n ->
            toString n

        BadGuess n ->
            toString n
