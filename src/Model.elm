module Model exposing (Model, initModel)

import Array
import Board exposing (Board)
import Player exposing (Player(..))
import GameStatus exposing (GameStatus)

type alias Model =
    { board : Board
    , gameStatus : GameStatus
    }

initModel : ( Model, Cmd msg )
initModel =
    ( { board =
            Array.initialize 3 <| always <| Array.initialize 3 <| always Nothing
      , gameStatus = GameStatus.Turn <| Player.X
      }
    , Cmd.none
    )