module Element.Custom exposing (..)

import Element exposing (..)
import Element.Custom.Attribute exposing (style)


inlineImage : List (Element.Attribute msg) -> { src : String, description : String } -> Element msg
inlineImage attrs desc =
    image
        (style "display" "inline-flex"
            :: attrs
        )
        desc
