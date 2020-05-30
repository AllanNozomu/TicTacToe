module Player exposing (Player(..), toString, changeTurn)

type Player 
    = X
    | O

toString : Maybe Player -> String
toString p =
    case p of
        Just X -> "X"
        Just O -> "O"
        Nothing -> ""

changeTurn : Bool -> Player -> Player
changeTurn win p =
    if win then
        p
    else if p == X then
        O
    else
        X