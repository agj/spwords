module Viewport exposing (..)

import Json.Decode as Decode exposing (Decoder, int)
import Json.Decode.Pipeline exposing (required)
import Ports


type alias Viewport =
    { width : Int
    , height : Int
    }


get : Cmd msg
get =
    Ports.getViewport ()


got : (Viewport -> msg) -> msg -> Sub msg
got success error =
    Ports.gotViewport <|
        \v ->
            case Decode.decodeValue decoder v of
                Ok vp ->
                    success vp

                Err _ ->
                    error


decoder : Decoder Viewport
decoder =
    Decode.succeed Viewport
        |> required "width" int
        |> required "height" int
