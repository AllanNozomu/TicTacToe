module Player exposing (Player(..), toString, changeTurn, toStringMaybe)

type Player 
    = X
    | O

toString : Player -> String
toString p =
    case p of
        X -> "X"
        O -> "O"

toStringMaybe : Maybe Player -> String
toStringMaybe m =
    case m of
        Just p -> toString p
        _ -> ""

changeTurn : Bool -> Player -> Player
changeTurn win p =
    if win then
        p
    else if p == X then
        O
    else
        X