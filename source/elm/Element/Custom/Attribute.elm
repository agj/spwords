module Element.Custom.Attribute exposing (..)

import Element exposing (..)
import Html.Attributes
import Html.Events
import Json.Decode as Decode
import Util.Element exposing (toCssColor)


backgroundColor : Element.Color -> Attribute msg
backgroundColor color =
    style "background-color" (toCssColor color)


iOsTextScalingFix : Attribute msg
iOsTextScalingFix =
    style "-webkit-text-size-adjust" "100%"


style : String -> String -> Attribute msg
style attribute value =
    Element.htmlAttribute <| Html.Attributes.style attribute value


id : String -> Attribute msg
id name =
    Element.htmlAttribute <| Html.Attributes.id name


onKey : String -> msg -> Attribute msg
onKey key msg =
    htmlAttribute
        (Html.Events.preventDefaultOn "keydown"
            (Decode.field "key" Decode.string
                |> Decode.andThen
                    (\k ->
                        if k == key then
                            Decode.succeed ( msg, True )

                        else
                            Decode.succeed ( msg, False )
                    )
            )
        )


raise : Int -> Attribute msg
raise n =
    style "z-index" (String.fromInt n)
