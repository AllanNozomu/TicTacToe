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
    , draw : Bool
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
    , draw = False
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

                isDraw =
                    checkDraw newBoard
            in
                { model | board = newBoard, turn = newTurn, winner = newWinner, draw = isDraw }


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
        if winnerCO || winnerRO || winnerDO then
            'O'
        else if winnerCX || winnerRX || winnerDX then
            'X'
        else
            ' '


checkDraw : List (List Tile) -> Bool
checkDraw board =
    let
        rows =
            List.filter
                (\row ->
                    (List.filter
                        (\cell ->
                            cell.value /= ' '
                        )
                        row
                        |> List.length
                    )
                        == 3
                )
                board
    in
        List.length rows == 3


checkDiagonals : List (List Tile) -> Char -> Bool
checkDiagonals board value =
    let
        diag1 =
            List.filter
                (\row ->
                    let
                        cells =
                            List.filter
                                (\cell ->
                                    (cell.value == value && cell.x == cell.y)
                                )
                                row
                    in
                        List.length cells == 1
                )
                board

        diag2 =
            List.filter
                (\row ->
                    let
                        cells =
                            (List.filter
                                (\cell ->
                                    (cell.value == value && cell.x == 2 && cell.y == 0)
                                        || (cell.value == value && cell.x == 1 && cell.y == 1)
                                        || (cell.value == value && cell.x == 0 && cell.y == 2)
                                )
                                row
                            )
                    in
                        List.length cells == 1
                )
                board
    in
        List.length diag1 == 3 || List.length diag2 == 3


columnAux : List (List Tile) -> Char -> Int -> Bool
columnAux board value numberCol =
    let
        rows =
            List.filter
                (\row ->
                    let
                        cells =
                            (List.filter
                                (\cell ->
                                    cell.value == value && cell.x == numberCol
                                )
                                row
                            )
                    in
                        List.length cells == 1
                )
                board
    in
        List.length rows == 3


checkColumn : List (List Tile) -> Char -> Bool
checkColumn board value =
    let
        nrows0 =
            columnAux board value 0

        nrows1 =
            columnAux board value 1

        nrows2 =
            columnAux board value 2
    in
        nrows0 || nrows1 || nrows2


checkRow : List (List Tile) -> Char -> Bool
checkRow board value =
    let
        cols =
            List.filter
                (\row ->
                    let
                        cells =
                            List.filter
                                (\cell ->
                                    cell.value == value
                                )
                                row
                    in
                        List.length cells == 3
                )
                board
    in
        List.length cols == 1


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



--View


view : Model -> Html Msg
view model =
    div []
        [ h1 [ class "titulo" ] [ text "Tic Tac Toe" ]
        , showWinner model
        , makeBoard model
        , clearButton
        ]


showWinner : Model -> Html Msg
showWinner model =
    h1 []
        [ if model.winner /= ' ' then
            text ("Winner = " ++ (String.fromChar model.winner))
          else if model.draw then
            text "DRAW!"
          else
            text (String.fromChar model.turn ++ "'s Turn")
        ]


clearButton : Html Msg
clearButton =
    button [ type' "button", onClick Clear ] [ text "Restart" ]


makeBoard : Model -> Html Msg
makeBoard model =
    div [ class "board" ]
        [ ul []
            (List.map
                (\boardRow ->
                    makeBoardCells boardRow
                )
                model.board
            )
        ]


makeBoardCells : List Tile -> Html Msg
makeBoardCells boardRow =
    li []
        (List.map
            (\cell ->
                div
                    [ class "button", onClick (Place cell.x cell.y) ]
                    [ text
                        (String.fromChar cell.value)
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
