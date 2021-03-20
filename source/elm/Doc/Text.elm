module Doc.Text exposing (Text, content, create, format, setContent)

import Doc.Format exposing (Format)


type Text
    = Text
        { content : String
        , format : Format
        }


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


setContent : String -> Text -> Text
setContent str (Text text) =
    Text { text | content = str }
