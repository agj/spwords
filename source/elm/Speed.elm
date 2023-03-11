module Speed exposing (..)

import Json.Decode as Decode
import Json.Encode as Encode


type Speed
    = Normal
    | Slow
    | VerySlow


encode : Speed -> Encode.Value
encode speed =
    case speed of
        Normal ->
            Encode.string "normal"

        Slow ->
            Encode.string "slow"

        VerySlow ->
            Encode.string "very-slow"


decoder : Decode.Decoder Speed
decoder =
    Decode.string
        |> Decode.andThen
            (\str ->
                case str of
                    "normal" ->
                        Decode.succeed Normal

                    "slow" ->
                        Decode.succeed Slow

                    "very-slow" ->
                        Decode.succeed VerySlow

                    _ ->
                        Decode.fail "Game speed not recognized."
            )
