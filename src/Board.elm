module Board exposing (Board, BoardLine, checkForWinner, checkDraw, updateCell)

import Player exposing (Player)
import Array exposing (Array)
import Utils exposing (Position)

type alias BoardLine = Array (Maybe Player)
type alias Board = Array (BoardLine)

checkForWinner : Board -> Bool
checkForWinner board =
    checkRow board || checkColumn board || checkDiagonals board


checkDraw : Board -> Bool
checkDraw board =
    not (board |> Array.map (\row -> Array.toList row) |> Array.toList |> List.concat |> List.member Nothing)

checkDiagonals : Board -> Bool
checkDiagonals board =
    let
        diagonal1 =
            board |> Array.indexedMap
                (\index row ->
                    Array.get index row |> Maybe.withDefault Nothing
                )
                
        diagonal2 =
            board |> Array.indexedMap
                (\index row ->
                    Array.get (Array.length board - index - 1) row |> Maybe.withDefault Nothing
                )
    in
    allEquals diagonal1 || allEquals diagonal2


checkRow : Board -> Bool
checkRow board =
    board 
        |> Array.map (\line -> allEquals line ) 
        |> Array.foldr (||) False

checkColumn : Board -> Bool
checkColumn board =
    Array.get 0 board
        |> Maybe.withDefault Array.empty 
        |> Array.indexedMap (\index _ ->
                Array.map (\row -> Array.get index row |> Maybe.withDefault Nothing
            ) board 
            |> allEquals ) 
        |> Array.foldr (||) False

allEquals : BoardLine -> Bool
allEquals a =
    case Array.get 0 a of
    Just Nothing -> 
        False
    Just l ->
        Array.foldr (\ele acc -> acc && ele == l) True a
    Nothing -> False


updateCell : Board -> Player -> Position -> Board
updateCell board turn (posy, posx) =
    case Array.get posx board of
        Just line ->
            Array.set posx (Array.set posy (Just turn) line) board

        Nothing ->
            board