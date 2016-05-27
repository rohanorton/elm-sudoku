module Sudoku exposing (..)

import Html exposing (..)
import Matrix exposing (Matrix)


-- Model


type alias Model =
    { sudoku : Sudoku
    }


type alias Sudoku =
    Matrix Cell


type Cell
    = Defined Int
    | Guessed Int
    | Empty


init : ( Model, Cmd Msg )
init =
    { sudoku = Matrix.empty
    }
        ! []



-- Update


type Msg
    = New
    | Solve


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        New ->
            model ! []

        Solve ->
            model ! []



-- Subscriptions


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- View


view : Model -> Html Msg
view model =
    text "rewriting..."
