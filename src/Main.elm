module Main exposing (..)

import Array exposing (Array)
import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import String



--Models


type alias Model =
    { board : Array (Array Char)
    , turn : Char
    , winner : Bool
    , draw : Bool
    }


initModel : ( Model, Cmd msg )
initModel =
    ( { board =
            Array.initialize 3 <| always <| Array.initialize 3 <| always ' '
      , turn = 'X'
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
                        updateCell model.board model.turn posx posy

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


checkForWinner : Array (Array Char) -> Bool
checkForWinner board =
    checkRow board || checkColumn board || checkDiagonals board


checkDraw : Array (Array Char) -> Bool
checkDraw board =
    not (board |> Array.map (\row -> Array.toList row) |> Array.toList |> List.concat  |>  List.member ' ')

checkDiagonals : Array (Array Char) -> Bool
checkDiagonals board =
    let
        diagonal1 =
            board |> Array.indexedMap
                (\index row ->
                    Array.get index row |> Maybe.withDefault ' '
                )
                
        diagonal2 =
            board |> Array.indexedMap
                (\index row ->
                    Array.get (Array.length board - index - 1) row |> Maybe.withDefault ' '
                )
    in
    allEquals diagonal1 || allEquals diagonal2


checkRow : Array (Array Char) -> Bool
checkRow board =
    board 
        |> Array.map (\line -> allEquals line ) 
        |> Array.foldr (||) False

checkColumn : Array (Array Char) -> Bool
checkColumn board =
    Array.get 0 board
        |> Maybe.withDefault Array.empty 
        |> Array.indexedMap (\index _ ->
                Array.map (\row -> Array.get index row |> Maybe.withDefault ' '
            ) board 
            |> allEquals ) 
        |> Array.foldr (||) False

allEquals : Array Char -> Bool
allEquals a =
    case Array.get 0 a of
    Just ' ' -> 
        False
    Just l ->
        Array.foldr (\ele acc -> acc && ele == l) True a
    Nothing -> False


updateCell : Array(Array Char) -> Char -> Int -> Int -> Array (Array Char)
updateCell board turn posy posx =
    case Array.get posx board of
        Just line ->
            Array.set posx (Array.set posy turn line) board

        Nothing ->
            board



--View


view : ( Model, Cmd msg ) -> Html Msg
view ( model, cmd ) =
    div []
        [ h1 [ class "titulo" ] [ text "Tic Tac Toe" ]
        , showHeader model.winner model.draw model.turn
        , makeBoard model.board
        , clearButton
        ]


showHeader : Bool -> Bool -> Char -> Html Msg
showHeader winner draw turn =
    h1 []
        [ if winner then
            text ("Winner = " ++ String.fromChar turn)

          else if draw then
            text "DRAW!"

          else
            text (String.fromChar turn ++ "'s Turn")
        ]


clearButton : Html Msg
clearButton =
    button [ type_ "button", onClick Clear ] [ text "Restart" ]


makeBoard : Array (Array Char) -> Html Msg
makeBoard board =
    div [ class "board" ]
        [ ul []
            (List.indexedMap
                (\rowIndex boardRow ->
                    makeBoardCells rowIndex <| Array.toList boardRow
                )
             <|
                Array.toList board
            )
        ]


makeBoardCells : Int -> List Char -> Html Msg
makeBoardCells y boardRow =
    li []
        (List.indexedMap
            (\x cell ->
                div
                    [ class "button"
                    , case cell of
                        'X' ->
                            class "red"

                        'O' ->
                            class "blue"

                        _ ->
                            onClick (Place x y)
                    ]
                    [ text
                        (String.fromChar cell)
                    ]
            )
            boardRow
        )


main =
    Browser.sandbox { init = initModel, update = update, view = view }
