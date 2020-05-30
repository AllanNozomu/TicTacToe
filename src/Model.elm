module Model exposing (Model)

import Player exposing (Player)
import Board exposing (Board)

type alias Model =
    { board : Board
    , turn : Player
    , winner : Bool
    , draw : Bool
    }
