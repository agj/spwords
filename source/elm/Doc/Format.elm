module Doc.Format exposing
    ( Format
    , athlete
    , empty
    , isBold
    , isItalic
    , isVar
    , link
    , setAthlete
    , setBold
    , setItalic
    , setLink
    , setVar
    )

import Athlete exposing (..)
import Doc.Link exposing (Link)


type Format
    = Format
        { bold : Bool
        , italic : Bool
        , var : Bool
        , athlete : Maybe Athlete
        , link : Maybe Link
        }


empty : Format
empty =
    Format
        { bold = False
        , italic = False
        , var = False
        , athlete = Nothing
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


athlete : Format -> Maybe Athlete
athlete (Format format) =
    format.athlete


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


setAthlete : Maybe Athlete -> Format -> Format
setAthlete athlete_ (Format format) =
    Format { format | athlete = athlete_ }


setLink : Maybe Link -> Format -> Format
setLink maybeLink (Format format) =
    Format { format | link = maybeLink }
