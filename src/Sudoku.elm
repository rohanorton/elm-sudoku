module Sudoku exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, on, targetValue)
import Matrix exposing (Matrix)
import Json.Decode as Json
import Array
import List
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
    | SetSquare Int Int String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg ({ sudoku } as model) =
    case Debug.log "" msg of
        New ->
            model ! []

        Solve ->
            model ! []

        SetSquare x y str ->
            let
                sudoku' =
                    guess x y str sudoku
            in
                { model | sudoku = sudoku' } ! []


guess : Int -> Int -> String -> Sudoku -> Sudoku
guess x y str sudoku =
    let
        val =
            check x y str sudoku
    in
        Matrix.set x y val sudoku


check : Int -> Int -> String -> Sudoku -> Cell
check x y str sudoku =
    let
        int =
            String.toInt str
                |> Result.toMaybe
                |> Maybe.withDefault 0
    in
        if int == 0 then
            Empty
        else
            GoodGuess int



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
    td [ class <| "cell " ++ cellClass cell ]
        [ input
            [ type' "text"
            , maxlength 1
            , value <| cellContentString cell
            , readonly <| isReadOnly cell
            , on "input" (Json.map (SetSquare x y) targetValue)
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
