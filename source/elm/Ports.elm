port module Ports exposing (..)

import Json.Encode exposing (Value)



-- OUTBOUND


port getViewport : () -> Cmd msg



-- INBOUND


port gotViewport : (Value -> msg) -> Sub msg
