module Update exposing (update, Msg(..))

import Model exposing (..)
import Board exposing (..)
import Player exposing (..)
import GameStatus exposing (..)
import Utils exposing (..)

type Msg
    = Clear
    | Place Position

update : Msg -> ( Model, Cmd msg ) -> ( Model, Cmd msg )
update msg ( model, cmd ) =
    case msg of
        Clear ->
            initModel

        Place pos ->
            case model.gameStatus of
                Turn currPlayer ->
                    let
                        newBoard =
                            Board.updateCell model.board currPlayer pos

                        newWinner =
                            Board.checkForWinner newBoard
                            
                        isDraw =
                            if newWinner then
                                False
                            else
                                Board.checkDraw newBoard

                        newStatus =
                            if newWinner then
                                Winner currPlayer
                            else if isDraw then
                                Draw
                            else
                                Turn <| Player.changeTurn newWinner currPlayer
                    in
                    ( { model
                        | board = newBoard
                        , gameStatus = newStatus
                    }
                    , Cmd.none
                    )
                _ -> 
                    (model, Cmd.none)