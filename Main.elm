module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Html.App as App
import String


--Models


type alias Model =
    { board : List (List Tile)
    , turn : Char
    }


type alias Tile =
    { x : Int
    , y : Int
    , value : Char
    }


initModel : Model
initModel =
    { board =
        [ [ Tile 0 0 ' ', Tile 1 0 ' ', Tile 2 0 ' ' ]
        , [ Tile 0 1 ' ', Tile 1 1 ' ', Tile 2 1 ' ' ]
        , [ Tile 0 2 ' ', Tile 1 2 ' ', Tile 2 2 ' ' ]
        ]
    , turn = 'X'
    }



--Update


type Msg
    = Clear
    | Place Int Int


update : Msg -> Model -> Model
update msg model =
    case msg of
        Clear ->
            initModel

        Place posx posy ->
            let
                newBoard =
                    (List.map
                        (\row ->
                            List.map
                                (\cell ->
                                    if cell.x == posx && cell.y == posy && cell.value == ' ' then
                                        { cell | value = model.turn }
                                    else
                                        cell
                                )
                                row
                        )
                        model.board
                    )

                newTurn =
                    if newBoard == model.board then
                        model.turn
                    else if model.turn == 'X' then
                        'O'
                    else
                        'X'
            in
                { model | board = newBoard, turn = newTurn }



--view


view : Model -> Html Msg
view model =
    div []
        [ h1 [] [ text "Tic Tac Toe" ]
        , makeBoard model
        , clearButton
        ]


clearButton : Html Msg
clearButton =
    button [ type' "button", onClick Clear ] [ text "Restart" ]


makeBoard : Model -> Html Msg
makeBoard model =
    ul []
        (List.map
            (\boardRow ->
                makeBoardCells boardRow
            )
            model.board
        )


makeBoardCells : List Tile -> Html Msg
makeBoardCells boardRow =
    li []
        (List.map
            (\cell ->
                div
                    -- (if cell.value /= 'X' && cell.value /= 'O' then
                    [ class "button", onClick (Place cell.x cell.y) ]
                    --  else
                    -- [ class "button" ]
                    -- )
                    [ text
                        (if cell.value == 'X' then
                            "X"
                         else if cell.value == 'O' then
                            "O"
                         else
                            "[]"
                        )
                    ]
            )
            boardRow
        )


main : Program Never
main =
    App.beginnerProgram
        { model = initModel
        , view = view
        , update = update
        }
