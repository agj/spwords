module Game.GameMode exposing (..)

import Json.Decode as Decode
import Json.Encode as Encode


type GameMode
    = HotseatMode
    | SingleMode


encode : GameMode -> Encode.Value
encode mode =
    case mode of
        HotseatMode ->
            Encode.string "hotseat"

        SingleMode ->
            Encode.string "single"


decoder : Decode.Decoder GameMode
decoder =
    Decode.string
        |> Decode.andThen
            (\str ->
                case str of
                    "hotseat" ->
                        Decode.succeed HotseatMode

                    "single" ->
                        Decode.succeed SingleMode

                    _ ->
                        Decode.fail "Game mode not recognized."
            )
