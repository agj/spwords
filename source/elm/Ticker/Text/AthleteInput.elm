module Ticker.Text.AthleteInput exposing (..)

import Ticker.Text.Constraints as Constraints exposing (Constraints)


type AthleteInput
    = Inputting String Constraints
    | Correct String
    | Wrong String
