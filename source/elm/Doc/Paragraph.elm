module Doc.Paragraph exposing
    ( Paragraph
    , content
    , create
    , empty
    , length
    , mapContent
    , toString
    )

import Doc.Text as Text exposing (Text)


type Paragraph
    = Paragraph (List Text)


empty : Paragraph
empty =
    Paragraph []


create : List Text -> Paragraph
create texts =
    Paragraph texts


content : Paragraph -> List Text
content (Paragraph texts) =
    texts


length : Paragraph -> Int
length (Paragraph texts) =
    texts
        |> List.map Text.length
        |> List.foldl (+) 0


mapContent : (List Text -> List Text) -> Paragraph -> Paragraph
mapContent mapper (Paragraph texts) =
    mapper texts |> Paragraph


toString : Paragraph -> String
toString (Paragraph texts) =
    texts
        |> List.map Text.content
        |> List.foldr (++) ""
