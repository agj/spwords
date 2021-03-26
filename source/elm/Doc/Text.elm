module Doc.Text exposing
    ( Text
    , content
    , create
    , empty
    , format
    , mapContent
    , mapFormat
    , setContent
    )

import Doc.Format as Format exposing (Format)


type Text
    = Text
        { content : String
        , format : Format
        }


empty : Text
empty =
    Text { content = "", format = Format.empty }


create : Format -> String -> Text
create fmt cnt =
    Text { content = cnt, format = fmt }



-- ACCESSORS


content : Text -> String
content (Text text) =
    text.content


format : Text -> Format
format (Text text) =
    text.format



-- MODIFIERS


mapContent : (String -> String) -> Text -> Text
mapContent mapper (Text text) =
    Text { text | content = mapper text.content }


mapFormat : (Format -> Format) -> Text -> Text
mapFormat mapper (Text text) =
    Text { text | format = mapper text.format }


setContent : String -> Text -> Text
setContent str (Text text) =
    Text { text | content = str }


setFormat : Format -> Text -> Text
setFormat format_ (Text text) =
    Text { text | format = format_ }
