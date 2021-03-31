module Passed exposing
    ( Passed
    , empty
    , push
    , toList
    )

import Message exposing (Message)


type Passed
    = Passed (List Message)


empty : Passed
empty =
    Passed []


push : Message -> Passed -> Passed
push mes (Passed list) =
    Passed (mes :: list)


toList : Passed -> List Message
toList (Passed list) =
    list
