module Main exposing (..)

import Html.App as Html
import Sudoku exposing (init, view, update, subscriptions)


main : Program Never
main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
