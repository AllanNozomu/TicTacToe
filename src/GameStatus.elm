module GameStatus exposing (GameStatus(..))

import Player exposing (Player)

type GameStatus
    = Turn Player
    | Winner Player
    | Draw

