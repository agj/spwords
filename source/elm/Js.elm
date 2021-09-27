port module Js exposing (saveState)

import Json.Encode as E
import SaveState exposing (SaveState)


saveState : SaveState -> Cmd msg
saveState state =
    command
        { kind = "saveState"
        , value = SaveState.encode state
        }



-- INTERNAL


type alias ToJs =
    { kind : String
    , value : E.Value
    }


port command : ToJs -> Cmd msg
