module Main (..) where

import StartApp.Simple exposing (start)
import Sudoku exposing (init, view, update)
import Html exposing (Html)


main : Signal.Signal Html
main =
  start
    { model = init
    , view = view
    , update = update
    }
