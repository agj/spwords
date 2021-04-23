module Viewport exposing (..)

import Json.Decode as Decode exposing (Decoder, int)
import Json.Decode.Pipeline exposing (required)


type alias Viewport =
    { width : Int
    , height : Int
    }


decoder : Decoder Viewport
decoder =
    Decode.succeed Viewport
        |> required "width" int
        |> required "height" int
