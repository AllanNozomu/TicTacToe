module View exposing (view)

import Array
import Board exposing (Board, BoardLine)
import GameStatus exposing (GameStatus(..))
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Model exposing (Model)
import Player exposing (Player(..))
import Update exposing (Msg(..))


view : ( Model, Cmd msg ) -> Html Msg
view ( model, _ ) =
    div [ class "container" ]
        [ div [ class "content" ]
            [ h1 [ class "titulo" ] [ text "Tic Tac Toe" ]
            , showHeader model.gameStatus
            , makeBoard model.board
            , clearButton
            ]
        , footer []
            [ p []
                [ text "Made with "
                , i [ class "devicon-elm-plain" ] []
                , text " by "
                , a [ href "https://github.com/allannozomu/tictactoe" ] [ text "allannozomu" ]
                ]
            ]
        ]


showHeader : GameStatus -> Html Msg
showHeader status =
    h1 []
        [ case status of
            Winner player ->
                text <| "Winner = " ++ Player.toString player

            Draw ->
                text "DRAW!"

            Turn player ->
                text (Player.toString player ++ "'s Turn")
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
             <|
                Array.toList board
            )
        ]


makeBoardCells : Int -> BoardLine -> Html Msg
makeBoardCells y boardRow =
    li []
        (List.indexedMap
            (\x cell ->
                div
                    (class "button"
                        :: (case cell of
                                Just Player.X ->
                                    [ class "red" ]

                                Just Player.O ->
                                    [ class "blue" ]

                                _ ->
                                    [ onClick <| Place ( x, y ), class "unplacedButton" ]
                           )
                    )
                    [ text <| Player.toStringMaybe cell ]
            )
         <|
            Array.toList boardRow
        )
