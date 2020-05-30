module Main exposing (..)

import Browser

import Model exposing (initModel)
import View exposing (view)
import Update exposing (update)


main =
    Browser.sandbox { init = initModel, update = update, view = view }
