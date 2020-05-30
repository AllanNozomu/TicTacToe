module Model exposing (Model, initModel)

import Array
import Player exposing (Player)
import Board exposing (Board)

type alias Model =
    { board : Board
    , turn : Player
    , winner : Bool
    , draw : Bool
    }

initModel : ( Model, Cmd msg )
initModel =
    ( { board =
            Array.initialize 3 <| always <| Array.initialize 3 <| always Nothing
      , turn = Player.X
      , winner = False
      , draw = False
      }
    , Cmd.none
    )