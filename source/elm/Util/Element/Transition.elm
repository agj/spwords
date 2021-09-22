module Util.Element.Transition exposing (..)

import Element exposing (htmlAttribute)
import Simple.Transition as Transition


all :
    { duration : Transition.Millis, options : List Transition.Option }
    -> List (Transition.Millis -> List Transition.Option -> Transition.Property)
    -> Element.Attribute msg
all options =
    Transition.all options >> htmlAttribute


each : List Transition.Property -> Element.Attribute msg
each props =
    Transition.properties props
        |> htmlAttribute
