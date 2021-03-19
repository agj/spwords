module Ticker exposing
    ( Ticker
    , current
    , empty
    , enter
    , input
    , inputWrong
    , inputted
    , passed
    , queueUp
    , tick
    )

import Ticker.Queued as Queued exposing (Queued)
import Ticker.Text as Text exposing (Active, Text)
import Ticker.Text.AthleteInput as AthleteInput exposing (AthleteInput)
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
        Just (Text.ActiveAthleteInput text cnst) ->
            Just text

        _ ->
            Nothing



-- MODIFICATION


queueUp : Queued.Queued -> Ticker -> Ticker
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
                advanceQueue (Text.Announcement (Text.InterruptedAnnouncement txt ticks)) ticker

            _ ->
                ticker


input : String -> Ticker -> Ticker
input text ticker =
    checkAdvanceQueue <|
        case current ticker of
            Just (Text.ActiveAthleteInput txt cnst) ->
                let
                    fixedText =
                        text
                            |> String.toLower
                            |> String.filter
                                (\ch ->
                                    String.any ((==) ch) "abcdefghijklmnopqrstuvwxyzñ-'"
                                )
                in
                swapCurrent (Text.ActiveAthleteInput (txt ++ fixedText) cnst) ticker

            _ ->
                ticker


inputWrong : Ticker -> Ticker
inputWrong ticker =
    case current ticker of
        Just (Text.ActiveAthleteInput text cnst) ->
            advanceQueue (Text.AthleteInput (AthleteInput.Wrong text)) ticker

        _ ->
            ticker



-- INTERNAL


checkAdvanceQueue : Ticker -> Ticker
checkAdvanceQueue ticker =
    case current ticker of
        Just (Text.ActiveAnnouncement text ticks) ->
            if ticks >= String.length text then
                advanceQueue (Text.Announcement (Text.FinishedAnnouncement text)) ticker

            else
                ticker

        Just (Text.ActiveInstruction text ticks) ->
            if ticks >= String.length text then
                advanceQueue (Text.Instruction (Text.FinishedInstruction text)) ticker

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
        Queued.Announcement text ->
            Text.ActiveAnnouncement text 0

        Queued.Instruction text ->
            Text.ActiveInstruction text 0

        Queued.AthleteInput ->
            Text.ActiveAthleteInput "" (Constraints.Serve { initial = 's' })


swapCurrent : Active -> Ticker -> Ticker
swapCurrent at ticker =
    case ticker of
        ActiveTicker list _ queue ->
            ActiveTicker list at queue

        IdleTicker _ ->
            ticker
