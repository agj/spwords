module Element.Cursor exposing
    ( allScroll
    , auto
    , cell
    , colResize
    , contextMenu
    , copy
    , crosshair
    , default
    , eResize
    , ewResize
    , grab
    , grabbing
    , help
    , move
    , nResize
    , neResize
    , neswResize
    , noDrop
    , none
    , notAllowed
    , nsResize
    , nwResize
    , nwseResize
    , pointer
    , progress
    , rowResize
    , sResize
    , seResize
    , swResize
    , text
    , url
    , wResize
    , wait
    , zoomIn
    , zoomOut
    )

import Element exposing (Attribute, htmlAttribute)
import Html.Attributes


allScroll : Attribute msg
allScroll =
    setCursor "all-scroll"


auto : Attribute msg
auto =
    setCursor "auto"


cell : Attribute msg
cell =
    setCursor "cell"


contextMenu : Attribute msg
contextMenu =
    setCursor "context-menu"


colResize : Attribute msg
colResize =
    setCursor "col-resize"


copy : Attribute msg
copy =
    setCursor "copy"


crosshair : Attribute msg
crosshair =
    setCursor "crosshair"


default : Attribute msg
default =
    setCursor "default"


eResize : Attribute msg
eResize =
    setCursor "e-resize"


ewResize : Attribute msg
ewResize =
    setCursor "ew-resize"


grab : Attribute msg
grab =
    setCursor "grab"


grabbing : Attribute msg
grabbing =
    setCursor "grabbing"


help : Attribute msg
help =
    setCursor "help"


move : Attribute msg
move =
    setCursor "move"


nResize : Attribute msg
nResize =
    setCursor "n-resize"


neResize : Attribute msg
neResize =
    setCursor "ne-resize"


neswResize : Attribute msg
neswResize =
    setCursor "nesw-resize"


nsResize : Attribute msg
nsResize =
    setCursor "ns-resize"


nwResize : Attribute msg
nwResize =
    setCursor "nw-resize"


nwseResize : Attribute msg
nwseResize =
    setCursor "nwse-resize"


noDrop : Attribute msg
noDrop =
    setCursor "no-drop"


none : Attribute msg
none =
    setCursor "none"


notAllowed : Attribute msg
notAllowed =
    setCursor "not-allowed"


pointer : Attribute msg
pointer =
    setCursor "pointer"


progress : Attribute msg
progress =
    setCursor "progress"


rowResize : Attribute msg
rowResize =
    setCursor "row-resize"


sResize : Attribute msg
sResize =
    setCursor "s-resize"


seResize : Attribute msg
seResize =
    setCursor "se-resize"


swResize : Attribute msg
swResize =
    setCursor "sw-resize"


text : Attribute msg
text =
    setCursor "text"


url : Attribute msg
url =
    setCursor "url"


wResize : Attribute msg
wResize =
    setCursor "w-resize"


wait : Attribute msg
wait =
    setCursor "wait"


zoomIn : Attribute msg
zoomIn =
    setCursor "zoom-in"


zoomOut : Attribute msg
zoomOut =
    setCursor "zoom-out"



-- INTERNAL


setCursor : String -> Attribute msg
setCursor value =
    htmlAttribute <| Html.Attributes.style "cursor" value
