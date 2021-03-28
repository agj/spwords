module Ticker exposing
    ( Ticker
    , current
    , empty
    , enter
    , input
    , inputCorrect
    , inputWrong
    , inputted
    , passed
    , queueUp
    , tick
    )

import Doc.Paragraph as Paragraph
import Ticker.Text as Text exposing (Active, Queued, Text)
import Ticker.Text.Constraints as Constraints exposing (Constraints)


type Ticker
    = ActiveTicker (List Text) Active (List Queued)
    | IdleTicker (List Text)



-- CONSTRUCTORS


empty : Ticker
empty =
    IdleTicker []



-- GETTERS


current : Ticker -> Maybe Active
current ticker =
    case ticker of
        ActiveTicker _ cur _ ->
            Just cur

        IdleTicker _ ->
            Nothing


passed : Ticker -> List Text
passed ticker =
    case ticker of
        ActiveTicker list _ _ ->
            list

        IdleTicker list ->
            list


inputted : Ticker -> Maybe String
inputted ticker =
    case current ticker of
        Just (Text.ActiveAthleteInput athlete text cnst) ->
            Just text

        _ ->
            Nothing



-- MODIFICATION


queueUp : Queued -> Ticker -> Ticker
queueUp q ticker =
    case ticker of
        ActiveTicker list cur queue ->
            ActiveTicker list cur (List.append queue [ q ])

        IdleTicker list ->
            ActiveTicker list (fromQueued q) []


tick : Ticker -> Ticker
tick ticker =
    checkAdvanceQueue <|
        case current ticker of
            Just (Text.ActiveAnnouncement text ticks) ->
                swapCurrent (Text.ActiveAnnouncement text (ticks + 1)) ticker

            Just (Text.ActiveInstruction text ticks) ->
                swapCurrent (Text.ActiveInstruction text (ticks + 1)) ticker

            _ ->
                ticker


enter : Ticker -> Ticker
enter ticker =
    checkAdvanceQueue <|
        case current ticker of
            Just (Text.ActiveAnnouncement txt ticks) ->
                advanceQueue (Text.InterruptedAnnouncement txt ticks) ticker

            _ ->
                ticker


input : String -> Ticker -> Ticker
input text ticker =
    checkAdvanceQueue <|
        case current ticker of
            Just (Text.ActiveAthleteInput athlete txt cnst) ->
                let
                    fixedText =
                        text
                            |> String.toLower
                            |> String.filter
                                (\ch ->
                                    String.any ((==) ch) "abcdefghijklmnopqrstuvwxyzñ-'"
                                )
                in
                swapCurrent (Text.ActiveAthleteInput athlete (txt ++ fixedText) cnst) ticker

            _ ->
                ticker


inputCorrect : Ticker -> Ticker
inputCorrect ticker =
    case current ticker of
        Just (Text.ActiveAthleteInput athlete text cnst) ->
            advanceQueue (Text.CorrectAthleteInput athlete text) ticker

        _ ->
            ticker


inputWrong : Ticker -> Ticker
inputWrong ticker =
    case current ticker of
        Just (Text.ActiveAthleteInput athlete text cnst) ->
            let
                fixedText =
                    if String.length text == 0 then
                        " "

                    else
                        text
            in
            advanceQueue (Text.WrongAthleteInput athlete fixedText) ticker

        _ ->
            ticker



-- INTERNAL


checkAdvanceQueue : Ticker -> Ticker
checkAdvanceQueue ticker =
    case current ticker of
        Just (Text.ActiveAnnouncement text ticks) ->
            if ticks >= String.length (Paragraph.toString text) then
                advanceQueue (Text.FinishedAnnouncement text) ticker

            else
                ticker

        Just (Text.ActiveInstruction text ticks) ->
            if ticks >= String.length (Paragraph.toString text) then
                advanceQueue (Text.Instruction text) ticker

            else
                ticker

        _ ->
            ticker


advanceQueue : Text -> Ticker -> Ticker
advanceQueue curReplacement ticker =
    case ticker of
        ActiveTicker list _ queue ->
            case List.head queue of
                Just new ->
                    ActiveTicker (curReplacement :: list) (fromQueued new) (List.tail queue |> Maybe.withDefault [])

                Nothing ->
                    IdleTicker (curReplacement :: list)

        IdleTicker _ ->
            ticker


fromQueued : Queued -> Active
fromQueued qt =
    case qt of
        Text.QueuedAnnouncement text ->
            Text.ActiveAnnouncement text 0

        Text.QueuedInstruction text ->
            Text.ActiveInstruction text 0

        Text.QueuedAthleteInput athlete cnts ->
            Text.ActiveAthleteInput athlete "" cnts


swapCurrent : Active -> Ticker -> Ticker
swapCurrent at ticker =
    case ticker of
        ActiveTicker list _ queue ->
            ActiveTicker list at queue

        IdleTicker _ ->
            ticker
