module Element.Custom.Input exposing (..)

import Element exposing (Element)
import Element.Custom.Attribute exposing (style)
import Element.Input as Input exposing (Label, Placeholder)


fixedMultiline : List (Element.Attribute msg) -> { text : String, onChange : String -> msg, placeholder : Maybe (Placeholder msg), label : Label msg, spellcheck : Bool } -> Element msg
fixedMultiline attrs desc =
    Input.multiline
        (style "height" "100%"
            :: attrs
        )
        desc
