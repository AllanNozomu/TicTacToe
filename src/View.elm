module View exposing (view)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)

import Array
import Model exposing (..)
import Board exposing (..)
import Player exposing (..)
import Update exposing (..)


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

