module Doc.Format exposing (Format, empty, isBold, isItalic, isVar, link, setBold, setItalic, setLink, setVar)

import Doc.Link exposing (Link)


type Format
    = Format
        { bold : Bool
        , italic : Bool
        , var : Bool
        , link : Maybe Link
        }


empty : Format
empty =
    Format
        { bold = False
        , italic = False
        , var = False
        , link = Nothing
        }


isBold : Format -> Bool
isBold (Format format) =
    format.bold


isItalic : Format -> Bool
isItalic (Format format) =
    format.italic


isVar : Format -> Bool
isVar (Format format) =
    format.var


link : Format -> Maybe Link
link (Format format) =
    format.link



-- SETTERS


setBold : Bool -> Format -> Format
setBold status (Format format) =
    Format { format | bold = status }


setItalic : Bool -> Format -> Format
setItalic status (Format format) =
    Format { format | italic = status }


setVar : Bool -> Format -> Format
setVar status (Format format) =
    Format { format | var = status }


setLink : Maybe Link -> Format -> Format
setLink maybeLink (Format format) =
    Format { format | link = maybeLink }
