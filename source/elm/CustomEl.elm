module CustomEl exposing (..)

import Element exposing (..)
import Html.Attributes as Attributes
import Html.Events
import Json.Decode as Decode
import Palette
import Utils exposing (..)



-- ELEMENTS


imageInline : List (Element.Attribute msg) -> { src : String, description : String } -> Element msg
imageInline attrs desc =
    image
        ([ style "display" "inline-flex"
         ]
            ++ attrs
        )
        desc



-- ATTRIBUTES


backgroundColor : Element.Color -> Element.Attribute msg
backgroundColor color =
    style "background-color" (toCssColor color)


iOsTextScalingFix : Element.Attribute msg
iOsTextScalingFix =
    style "-webkit-text-size-adjust" "100%"


style : String -> String -> Element.Attribute msg
style attribute value =
    Element.htmlAttribute <| Attributes.style attribute value


id : String -> Element.Attribute msg
id name =
    Element.htmlAttribute <| Attributes.id name


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
