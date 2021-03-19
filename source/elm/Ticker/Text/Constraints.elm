module Ticker.Text.Constraints exposing (..)


type Constraints
    = Serve
        { initial : Char
        }
    | Rally
        { initial : Char
        , includes : Char
        }
