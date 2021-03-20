module Doc.EmuDecode exposing (emuDecoder, fromEmu)

import Doc exposing (Doc)
import Doc.Format as Format exposing (Format)
import Doc.Link
import Doc.Paragraph as Paragraph exposing (Paragraph)
import Doc.Text as Text exposing (Text)
import Json.Decode as Decode exposing (Decoder, andThen, string)
import Mark
import Mark.Error
import Utils exposing (..)


fromEmu : String -> Doc
fromEmu raw =
    let
        withErrors errors =
            List.map errorToParagraph errors
                |> Doc.create
    in
    case Mark.compile emuDocument raw of
        Mark.Success result ->
            result

        Mark.Almost { result, errors } ->
            withErrors errors

        Mark.Failure errors ->
            withErrors errors


emuDecoder : Decoder Doc
emuDecoder =
    string
        |> andThen (\raw -> Decode.succeed (fromEmu raw))



-- INTERNAL


errorToParagraph : Mark.Error.Error -> Paragraph
errorToParagraph error =
    let
        doit string =
            Text.create (Format.empty |> Format.setCode True) string
                |> List.singleton
                |> Paragraph.create
    in
    Mark.Error.toString error |> doit


emuDocument : Mark.Document Doc
emuDocument =
    Mark.document emuWrapper <|
        Mark.manyOf
            [ Mark.map (unnest >> Paragraph.create) inlineParser
            ]


emuWrapper : List Paragraph -> Doc
emuWrapper =
    Doc.create


inlineParser : Mark.Block (List (List Text))
inlineParser =
    Mark.textWith
        { view = \styles str -> [ toFormattedText styles str ]
        , replacements = []
        , inlines =
            [ Mark.annotation "link" toLinkedText
                |> Mark.field "url" Mark.string
            , Mark.verbatim "code" toCodeText
            ]
        }


toCodeText : String -> List Text
toCodeText str =
    [ Text.create (Format.empty |> Format.setCode True) str ]


toFormattedText : Mark.Styles -> String -> Text
toFormattedText styles str =
    Text.create (toFormat styles) str


toLinkedText : List ( Mark.Styles, String ) -> String -> List Text
toLinkedText strings url =
    let
        process ( styles, str ) =
            Text.create
                (toFormat styles |> Format.setLink (Just (Doc.Link.create url)))
                str
    in
    List.map process strings


toFormat : Mark.Styles -> Format
toFormat styles =
    Format.empty
        |> Format.setBold styles.bold
        |> Format.setItalic styles.italic
