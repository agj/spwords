module Element.Events.Pointer exposing (Event, onDown, onPrimaryDown)

import Element exposing (htmlAttribute)
import Html.Events.Extra.Pointer as Pointer


type alias Event =
    Pointer.Event


onDown : (Pointer.Event -> msg) -> Element.Attribute msg
onDown handler =
    Pointer.onDown handler
        |> htmlAttribute


onPrimaryDown : msg -> msg -> Element.Attribute msg
onPrimaryDown msgNo msgYes =
    onDown (ifPrimary msgNo msgYes)



-- INTERNAL


ifPrimary : msg -> msg -> Pointer.Event -> msg
ifPrimary msgNo msgYes event =
    if event.isPrimary then
        msgYes

    else
        msgNo
