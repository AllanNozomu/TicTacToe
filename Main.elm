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
    , winner : Char
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
    , winner = ' '
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
                    if model.winner == ' ' then
                        updateCell model posx posy
                    else
                        model.board

                newTurn =
                    if newBoard == model.board then
                        model.turn
                    else if model.turn == 'X' then
                        'O'
                    else
                        'X'

                newWinner =
                    checkForWinner newBoard
            in
                { model | board = newBoard, turn = newTurn, winner = newWinner }


checkForWinner : List (List Tile) -> Char
checkForWinner newBoard =
    let
        winnerRX =
            checkRow newBoard 'X'

        winnerRO =
            checkRow newBoard 'O'

        winnerCX =
            checkColumn newBoard 'X'

        winnerCO =
            checkColumn newBoard 'O'

        winnerDX =
            checkDiagonals newBoard 'X'

        winnerDO =
            checkDiagonals newBoard 'O'
    in
        if winnerCO /= ' ' || winnerRO /= ' ' || winnerDO /= ' ' then
            'O'
        else if winnerRX /= ' ' || winnerRX /= ' ' || winnerDX /= ' ' then
            'X'
        else
            ' '


checkDiagonals : List (List Tile) -> Char -> Char
checkDiagonals board value =
    let
        diag1 =
            List.filter
                (\row ->
                    let
                        cells =
                            List.length
                                (List.filter
                                    (\cell ->
                                        (cell.value == value && cell.x == cell.y)
                                    )
                                    row
                                )
                    in
                        (cells == 1)
                )
                board

        diag2 =
            List.filter
                (\row ->
                    let
                        cells =
                            List.length
                                (List.filter
                                    (\cell ->
                                        (cell.value == value && cell.x == 2 && cell.y == 0)
                                            || (cell.value == value && cell.x == 1 && cell.y == 1)
                                            || (cell.value == value && cell.x == 0 && cell.y == 2)
                                    )
                                    row
                                )
                    in
                        (cells == 1)
                )
                board
    in
        if List.length diag1 == 3 || List.length diag2 == 3 then
            value
        else
            ' '


checkColumn : List (List Tile) -> Char -> Char
checkColumn board value =
    let
        rows =
            List.filter
                (\list ->
                    case List.head list of
                        Just cell ->
                            cell.value == value

                        Nothing ->
                            False
                )
                board
    in
        if List.length rows == 3 then
            value
        else
            ' '


checkRow : List (List Tile) -> Char -> Char
checkRow board value =
    let
        cols =
            List.filter
                (\row ->
                    let
                        cells =
                            List.length
                                (List.filter
                                    (\cell ->
                                        cell.value == value
                                    )
                                    row
                                )
                    in
                        (cells == 3)
                )
                board
    in
        if List.length cols == 1 then
            value
        else
            ' '


updateCell : Model -> Int -> Int -> List (List Tile)
updateCell model posx posy =
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



--view


view : Model -> Html Msg
view model =
    div []
        [ h1 [] [ text "Tic Tac Toe" ]
        , showWinner model
        , makeBoard model
        , clearButton
        ]


showWinner : Model -> Html Msg
showWinner model =
    h1 []
        [ if model.winner /= ' ' then
            text ("Winner = " ++ toString (model.winner))
          else
            text ""
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
