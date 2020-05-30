module Update exposing (update, Msg(..))

import Model exposing (..)
import Board exposing (..)
import Player exposing (..)

type Msg
    = Clear
    | Place Int Int

update : Msg -> ( Model, Cmd msg ) -> ( Model, Cmd msg )
update msg ( model, cmd ) =
    case msg of
        Clear ->
            initModel

        Place posx posy ->
            if model.winner then
                ( model, Cmd.none )

            else
                let
                    newBoard =
                        Board.updateCell model.board model.turn posx posy

                    newWinner =
                        Board.checkForWinner newBoard

                    isDraw =
                        if newWinner then
                            False

                        else
                            Board.checkDraw newBoard
                in
                ( { model
                    | board = newBoard
                    , turn = Player.changeTurn newWinner model.turn
                    , winner = newWinner
                    , draw = isDraw
                  }
                , Cmd.none
                )