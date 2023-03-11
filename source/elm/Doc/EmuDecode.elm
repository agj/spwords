module Doc.EmuDecode exposing (emuDecoder, fromEmu)

import Athlete exposing (..)
import Doc exposing (Doc)
import Doc.Format as Format exposing (Format)
import Doc.Link
import Doc.Paragraph as Paragraph exposing (Paragraph)
import Doc.Text as Text exposing (Text)
import Json.Decode as Decode exposing (Decoder, andThen, string)
import Mark
import Mark.Error
import Util.List as List


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
            errors
                |> withErrors

        Mark.Failure errors ->
            errors
                |> withErrors


emuDecoder : Decoder Doc
emuDecoder =
    string
        |> andThen (\raw -> Decode.succeed (fromEmu raw))



-- INTERNAL


errorToParagraph : Mark.Error.Error -> Paragraph
errorToParagraph error =
    let
        doit string =
            Text.create Format.empty string
                |> List.singleton
                |> Paragraph.create
    in
    Mark.Error.toString error |> doit


emuDocument : Mark.Document Doc
emuDocument =
    Mark.document Doc.create <|
        Mark.manyOf
            [ Mark.map (List.unnest >> Paragraph.create) inlineParser
            ]


inlineParser : Mark.Block (List (List Text))
inlineParser =
    Mark.textWith
        { view = \styles str -> [ toFormattedText styles str ]
        , replacements = []
        , inlines =
            [ Mark.annotation "link" toLinkedText
                |> Mark.field "url" Mark.string
            , Mark.annotation "athleteA" (toAthleteText AthleteA)
            , Mark.annotation "athleteB" (toAthleteText AthleteB)
            , Mark.annotation "invert" inverted
            , Mark.verbatim "var" toVarText
            ]
        }


toAthleteText : Athlete -> List ( Mark.Styles, String ) -> List Text
toAthleteText athlete strings =
    let
        process ( styles, str ) =
            Text.create
                (toFormat styles |> Format.setAthlete (Just athlete))
                str
    in
    List.map process strings


inverted : List ( Mark.Styles, String ) -> List Text
inverted strings =
    let
        process ( styles, str ) =
            Text.create
                (toFormat styles |> Format.setInverted True)
                str
    in
    List.map process strings


toFormattedText : Mark.Styles -> String -> Text
toFormattedText styles str =
    Text.create (toFormat styles) str


toVarText : String -> List Text
toVarText str =
    [ Text.create (Format.empty |> Format.setVar True) str ]


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
