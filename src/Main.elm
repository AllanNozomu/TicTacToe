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


checkForWinner : Array (Array Char) -> Bool
checkForWinner board =
    checkRow board || checkColumn board || checkDiagonals board


checkDraw : Array (Array Char) -> Bool
checkDraw board =
    not (board |> Array.map (\row -> Array.toList row) |> Array.toList |> List.concat  |>  List.member ' ')

checkDiagonals : Array (Array Char) -> Bool
checkDiagonals board =
    let
        spaceEqualNothing : Maybe Char -> Char
        spaceEqualNothing a =
            case a of
                Just b -> b
                Nothing -> ' '

        diagonal1 =
            board |> Array.indexedMap
                (\index row ->
                    Array.get index row |> spaceEqualNothing
                )
                

        diagonal2 =
            board |> Array.indexedMap
                (\index row ->
                    Array.get (Array.length board - index - 1) row |> spaceEqualNothing
                )
                
    in
    allEquals diagonal1 || allEquals diagonal2


checkRow : Array (Array Char) -> Bool
checkRow board =
    board |> Array.map
        (\line ->
            allEquals line
        ) |> Array.foldr (||) False

checkColumn : Array (Array Char) -> Bool
checkColumn board =
    let
        newLine = 
            case Array.get 0 board of
            Just a -> a
            Nothing -> Array.empty
    in
    newLine |> Array.indexedMap (\index _ ->
        Array.map (\row ->
            case Array.get index row of
                Just l -> l
                _ -> ' '
        ) board |> allEquals
    ) |> Array.foldr (||) False

allEquals : Array Char -> Bool
allEquals a =
    case Array.get 0 a of
    Just ' ' -> 
        False
    Just l ->
        Array.foldr (\ele acc -> acc && ele == l) True a
    Nothing -> False


updateCell : Model -> Int -> Int -> Array (Array Char)
updateCell model posy posx =
    let
        board =
            model.board

        turn =
            model.turn
    in
    case Array.get posx board of
        Just line ->
            Array.set posx (Array.set posy turn line) board

        Nothing ->
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
                    makeBoardCells rowIndex <| Array.toList boardRow
                )
             <|
                Array.toList model.board
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
