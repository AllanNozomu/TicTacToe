module Main exposing (..)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import String



--Models


type alias Model =
    { board : List (List Char)
    , turn : Char
    , winner : Bool
    , draw : Bool
    , columns : List (List Char)
    , diagonals : List (List Char)
    }


initModel : ( Model, Cmd msg )
initModel =
    ( { board =
            [ [ ' ', ' ', ' ' ]
            , [ ' ', ' ', ' ' ]
            , [ ' ', ' ', ' ' ]
            ]
      , turn = 'X'
      , winner = False
      , draw = False
      , columns =
            [ [ ' ', ' ', ' ' ]
            , [ ' ', ' ', ' ' ]
            , [ ' ', ' ', ' ' ]
            ]
      , diagonals =
            [ [ ' ', ' ', ' ' ]
            , [ ' ', ' ', ' ' ]
            ]
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
                        updateCell model posx posy

                    newWinner =
                        checkForWinner newBoard

                    isDraw =
                        if newWinner then
                            False

                        else
                            checkDraw newBoard

                    newTurn =
                        if newWinner then
                            model.turn

                        else if model.turn == 'X' then
                            'O'

                        else
                            'X'
                in
                ( { model
                    | board = newBoard
                    , turn = newTurn
                    , winner = newWinner
                    , draw = isDraw
                  }
                , Cmd.none
                )


checkForWinner : List (List Char) -> Bool
checkForWinner board =
    checkRow board || checkColumn board || checkDiagonals board


checkDraw : List (List Char) -> Bool
checkDraw board =
    not (List.concat board |> List.member ' ')


allEqual : List Char -> Bool
allEqual list =
    let
        value =
            takeElement 0 list ' '
    in
    List.foldr
        (\element status ->
            if status && not (element == ' ') then
                element == value
            else
                False
        )
        True
        list


takeElement : Int -> List a -> a -> a
takeElement index list default =
    case List.drop index list |> List.head of
        Just l ->
            l
        Nothing ->
            default


checkDiagonals : List (List Char) -> Bool
checkDiagonals board =
    let
        diagonal1 =
            List.indexedMap
                (\index row ->
                    takeElement index row ' '
                )
                board

        diagonal2 =
            List.indexedMap
                (\index row ->
                    takeElement (2 - index) row ' '
                )
                board
    in
    allEqual diagonal1 || allEqual diagonal2


checkColumn : List (List Char) -> Bool
checkColumn board =
    List.map allEqual board |> List.foldr (||) False


checkRow : List (List Char) -> Bool
checkRow board =
    let
        rowsRes =
            List.length board
                |> List.range 0
                |> List.map
                    (\col ->
                        List.map
                            (\row ->
                                takeElement col row ' '
                            )
                            board
                            |> allEqual
                    )
    in
    rowsRes |> List.foldr (||) False


updateCell : Model -> Int -> Int -> List (List Char)
updateCell model posx posy =
    List.indexedMap
        (\y row ->
            List.indexedMap
                (\x cell ->
                    if x == posx && y == posy && cell == ' ' then
                        model.turn

                    else
                        cell
                )
                row
        )
        model.board



--View


view : ( Model, Cmd msg ) -> Html Msg
view ( model, cmd ) =
    div []
        [ h1 [ class "titulo" ] [ text "Tic Tac Toe" ]
        , showWinner model
        , makeBoard model
        , clearButton
        ]


showWinner : Model -> Html Msg
showWinner model =
    h1 []
        [ if model.winner then
            text ("Winner = " ++ String.fromChar model.turn)

          else if model.draw then
            text "DRAW!"

          else
            text (String.fromChar model.turn ++ "'s Turn")
        ]


clearButton : Html Msg
clearButton =
    button [ type_ "button", onClick Clear ] [ text "Restart" ]


makeBoard : Model -> Html Msg
makeBoard model =
    div [ class "board" ]
        [ ul []
            (List.indexedMap
                (\rowIndex boardRow ->
                    makeBoardCells rowIndex boardRow
                )
                model.board
            )
        ]


makeBoardCells : Int -> List Char -> Html Msg
makeBoardCells y boardRow =
    li []
        (List.indexedMap
            (\x cell ->
                div
                    [ class "button",
                    case cell of 
                        'X' -> class "red"
                        'O' -> class "blue"
                        _ -> onClick (Place x y)
                    ]
                    [ text
                        (String.fromChar cell)
                    ]
            )
            boardRow
        )


main =
    Browser.sandbox { init = initModel, update = update, view = view }
