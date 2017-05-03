module Main exposing (..)

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
      }
    , Cmd.none
    )



--Update


type Msg
    = Clear
    | Place Int Int


update : Msg -> Model -> ( Model, Cmd msg )
update msg model =
    case msg of
        Clear ->
            initModel

        Place posx posy ->
            if model.winner then
                model ! []
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
                    { model | board = newBoard, turn = newTurn, winner = newWinner, draw = isDraw } ! []


checkForWinner : List (List Char) -> Bool
checkForWinner board =
    (checkRow board || checkColumn board || checkDiagonals board)


checkDraw : List (List Char) -> Bool
checkDraw board =
    not <| List.member ' ' (List.foldr List.append [] board)


allEqual : List Char -> Bool
allEqual list =
    case List.head list of
        Just ' ' ->
            False

        Just value ->
            List.foldr
                (\element status ->
                    if status then
                        element == value
                    else
                        False
                )
                True
                list

        Nothing ->
            False


takeElement : Int -> List a -> List a
takeElement index list =
    List.take 1 <| List.drop index list


checkDiagonals : List (List Char) -> Bool
checkDiagonals board =
    let
        diagonal1 =
            List.concat <|
                List.indexedMap
                    (\index row ->
                        takeElement index row
                    )
                <|
                    board

        diagonal2 =
            List.concat <|
                List.indexedMap
                    (\index row ->
                        takeElement (2 - index) row
                    )
                <|
                    board
    in
        (allEqual diagonal1 || allEqual diagonal2)


checkColumn : List (List Char) -> Bool
checkColumn board =
    List.length (List.filter (\status -> status) <| List.map allEqual board) > 0


checkRow : List (List Char) -> Bool
checkRow board =
    let
        rows =
            List.indexedMap
                (\index _ ->
                    List.concat <|
                        List.map
                            (\row ->
                                takeElement index row
                            )
                            board
                )
            <|
                List.range 0 2
    in
        List.length (List.filter (\status -> status) <| List.map allEqual rows) > 0


updateCell : Model -> Int -> Int -> List (List Char)
updateCell model posx posy =
    (List.indexedMap
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
        [ if model.winner then
            text ("Winner = " ++ (String.fromChar model.turn))
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
                    [ class "button", onClick (Place x y) ]
                    [ text
                        (String.fromChar cell)
                    ]
            )
            boardRow
        )


main : Program Never Model Msg
main =
    Html.program
        { init = initModel
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }
