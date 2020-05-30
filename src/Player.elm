module Player exposing (..)

type Player 
    = X
    | O

toString : Maybe Player -> String
toString p =
    case p of
        Just X -> "X"
        Just O -> "O"
        Nothing -> ""