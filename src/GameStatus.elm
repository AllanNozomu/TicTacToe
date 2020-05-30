module GameStatus exposing (..)

import Player exposing (..)

type GameStatus
    = Turn Player
    | Winner Player
    | Draw

