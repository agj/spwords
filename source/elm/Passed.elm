module Passed exposing
    ( Passed
    , empty
    )

import Message exposing (Message)


type Passed
    = Passed (List Message)


empty : Passed
empty =
    Passed []
