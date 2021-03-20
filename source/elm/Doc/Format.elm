module Doc.Format exposing (Format, empty, isBold, isCode, isItalic, link, setBold, setCode, setItalic, setLink)

import Doc.Link exposing (Link)


type Format
    = Format
        { bold : Bool
        , italic : Bool
        , code : Bool
        , link : Maybe Link
        }


empty : Format
empty =
    Format
        { bold = False
        , italic = False
        , code = False
        , link = Nothing
        }


isBold : Format -> Bool
isBold (Format format) =
    format.bold


isItalic : Format -> Bool
isItalic (Format format) =
    format.italic


isCode : Format -> Bool
isCode (Format format) =
    format.code


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


setCode : Bool -> Format -> Format
setCode status (Format format) =
    Format { format | code = status }


setLink : Maybe Link -> Format -> Format
setLink maybeLink (Format format) =
    Format { format | link = maybeLink }
