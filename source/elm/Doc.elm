module Doc exposing (Doc, content, create)

import Doc.Paragraph exposing (Paragraph)


type Doc
    = Doc (List Paragraph)


content : Doc -> List Paragraph
content (Doc paragraphs) =
    paragraphs


create : List Paragraph -> Doc
create paragraphs =
    Doc paragraphs
