module SaveState exposing (SaveState, decoder, encode)

import Game.GameMode as GameMode exposing (GameMode)
import Json.Decode as Decode exposing (Decoder, Value)
import Json.Decode.Pipeline exposing (required)
import Json.Encode as Encode
import Speed exposing (Speed)


type alias SaveState =
    { speed : Speed
    , mode : GameMode
    }


encode : SaveState -> Value
encode state =
    Encode.object
        [ ( "speed", Speed.encode state.speed )
        , ( "mode", GameMode.encode state.mode )
        ]
        |> Encode.encode 0
        |> Encode.string


decoder : Decoder SaveState
decoder =
    let
        toResult =
            Decode.decodeString
                (Decode.succeed SaveState
                    |> required "speed" Speed.decoder
                    |> required "mode" GameMode.decoder
                )

        resultDecoder res =
            case res of
                Ok ss ->
                    Decode.succeed ss

                Err _ ->
                    Decode.fail "error"
    in
    Decode.string
        |> Decode.andThen (toResult >> resultDecoder)
