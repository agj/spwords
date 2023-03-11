module Doc.Util exposing
    ( paragraphAppend
    , paragraphLeft
    , textAppend
    , textLeft
    , textLength
    )

import Doc.Format as Format exposing (Format)
import Doc.Paragraph as Paragraph exposing (Paragraph)
import Doc.Text as Text exposing (Text)
import List.Extra



-- PARAGRAPH


paragraphLeft : Int -> Paragraph -> Paragraph
paragraphLeft amount par =
    Paragraph.content par
        |> textListLeft amount
        |> Paragraph.create


paragraphAppend : String -> Paragraph -> Paragraph
paragraphAppend str par =
    let
        texts =
            Paragraph.content par

        init_ =
            List.Extra.init texts |> Maybe.withDefault []

        last =
            List.Extra.last texts |> Maybe.withDefault Text.empty
    in
    List.append init_ [ last |> textAppend str ]
        |> Paragraph.create



-- TEXT


textLength : Text -> Int
textLength txt =
    Text.content txt |> String.length


textLeft : Int -> Text -> Text
textLeft amount txt =
    let
        content =
            Text.content txt
    in
    Text.create
        (Text.format txt)
        (String.left amount content)


textAppend : String -> Text -> Text
textAppend str txt =
    txt
        |> Text.mapContent (\old -> old ++ str)



-- INTERNAL


textListLeft : Int -> List Text -> List Text
textListLeft amount texts =
    case texts of
        txt :: rest ->
            let
                txtLength =
                    textLength txt
            in
            if amount > txtLength then
                txt
                    :: textListLeft
                        (amount - txtLength)
                        rest

            else
                [ textLeft amount txt ]

        [] ->
            texts
