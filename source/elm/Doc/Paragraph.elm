module Doc.Paragraph exposing
    ( Paragraph
    , content
    , create
    , toString
    )

import Doc.Text exposing (Text)


type Paragraph
    = Paragraph (List Text)


create : List Text -> Paragraph
create texts =
    Paragraph texts


content : Paragraph -> List Text
content (Paragraph texts) =
    texts


toString : Paragraph -> String
toString (Paragraph texts) =
    texts
        |> List.map Doc.Text.content
        |> List.foldr (++) ""
