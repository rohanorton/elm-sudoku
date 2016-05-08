module Main (..) where

import StartApp exposing (start)
import Sudoku exposing (init, view, update)
import Html exposing (Html)


app =
  start
    { init = init
    , view = view
    , update = update
    , inputs = []
    }


main : Signal.Signal Html
main =
  app.html
