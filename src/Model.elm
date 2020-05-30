module Model exposing (..)

import Array exposing (Array)
import Player exposing (Player)

type alias Model =
    { board : Array (Array (Maybe Player))
    , turn : Player
    , winner : Bool
    , draw : Bool
    }
