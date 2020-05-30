module Main exposing (..)

import Array exposing (Array)
import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)

import Model exposing (Model)
import Player exposing (Player, Player(..))
import Board exposing (Board, BoardLine)


--Models

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



--Update


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


--View


view : ( Model, Cmd msg ) -> Html Msg
view ( model, cmd ) =
    div []
        [ h1 [ class "titulo" ] [ text "Tic Tac Toe" ]
        , showHeader model.winner model.draw model.turn
        , makeBoard model.board
        , clearButton
        ]


showHeader : Bool -> Bool -> Player -> Html Msg
showHeader winner draw turn =
    h1 []
        [ if winner then
            text <| "Winner = " ++ (Player.toString <| Just turn)

          else if draw then
            text "DRAW!"

          else
            text (Player.toString (Just turn) ++ "'s Turn")
        ]


clearButton : Html Msg
clearButton =
    button [ type_ "button", onClick Clear ] [ text "Restart" ]


makeBoard : Board -> Html Msg
makeBoard board =
    div [ class "board" ]
        [ ul []
            (List.indexedMap
                (\rowIndex boardRow ->
                    makeBoardCells rowIndex <| boardRow
                )
             <| Array.toList board
            )
        ]


makeBoardCells : Int -> BoardLine -> Html Msg
makeBoardCells y boardRow =
    li []
        (List.indexedMap
            (\x cell ->
                div
                    [ class "button"
                    , case cell of
                        Just Player.X ->
                            class "red"

                        Just Player.O ->
                            class "blue"

                        _ ->
                            onClick (Place x y)
                    ]
                    [ text
                        <| Player.toString cell
                    ]
            )
            <| Array.toList boardRow
        )


main =
    Browser.sandbox { init = initModel, update = update, view = view }
