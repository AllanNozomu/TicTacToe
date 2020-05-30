module Model exposing (..)

import Array exposing (Array)
import Player exposing (Player)

type alias Model =
    { board : Board
    , turn : Player
    , winner : Bool
    , draw : Bool
    }

type alias BoardLine = Array (Maybe Player)
type alias Board = Array (BoardLine)