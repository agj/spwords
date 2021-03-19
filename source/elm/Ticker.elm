module Ticker exposing
    ( Ticker
    , current
    , empty
    , enter
    , input
    , inputWrong
    , inputted
    , queueUp
    , tick
    , ticking
    , toList
    )

import Ticker.Queued as Queued exposing (Queued)
import Ticker.Text as Text exposing (Text)
import Ticker.Text.AthleteInput as AthleteInput exposing (AthleteInput)
import Ticker.Text.Constraints as Constraints exposing (Constraints)


type Ticker
    = ActiveTicker (List Text) Text (List Queued)
    | IdleTicker (List Text)



-- CONSTRUCTORS


empty : Ticker
empty =
    IdleTicker []



-- GETTERS


current : Ticker -> Maybe Text
current ticker =
    case ticker of
        ActiveTicker _ cur _ ->
            Just cur

        IdleTicker _ ->
            Nothing


toList : Ticker -> List Text
toList ticker =
    case ticker of
        ActiveTicker list cur _ ->
            cur :: list

        IdleTicker list ->
            list


ticking : Ticker -> Bool
ticking ticker =
    case current ticker of
        Just (Text.Announcement (Text.TickingAnnouncement _ _)) ->
            True

        _ ->
            False


inputted : Ticker -> Maybe String
inputted ticker =
    case current ticker of
        Just (Text.AthleteInput (AthleteInput.Inputting text cnst)) ->
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
            Just (Text.Announcement (Text.TickingAnnouncement text ticks)) ->
                if ticks < String.length text then
                    swapCurrent (Text.Announcement (Text.TickingAnnouncement text (ticks + 1))) ticker

                else
                    swapCurrent (Text.Announcement (Text.FinishedAnnouncement text)) ticker

            Just (Text.Instruction (Text.TickingInstruction text ticks)) ->
                if ticks < String.length text then
                    swapCurrent (Text.Instruction (Text.TickingInstruction text (ticks + 1))) ticker

                else
                    swapCurrent (Text.Instruction (Text.FinishedInstruction text)) ticker

            _ ->
                ticker


enter : Ticker -> Ticker
enter ticker =
    checkAdvanceQueue <|
        case current ticker of
            Just (Text.Announcement (Text.TickingAnnouncement txt ticks)) ->
                swapCurrent (Text.Announcement (Text.InterruptedAnnouncement txt ticks)) ticker

            _ ->
                ticker


input : String -> Ticker -> Ticker
input text ticker =
    checkAdvanceQueue <|
        case current ticker of
            Just (Text.AthleteInput (AthleteInput.Inputting txt cnst)) ->
                let
                    fixedText =
                        text
                            |> String.toLower
                            |> String.filter
                                (\ch ->
                                    String.any ((==) ch) "abcdefghijklmnopqrstuvwxyzñ-'"
                                )
                in
                swapCurrent (Text.AthleteInput (AthleteInput.Inputting (txt ++ fixedText) cnst)) ticker

            _ ->
                ticker


inputWrong : Ticker -> Ticker
inputWrong ticker =
    case current ticker of
        Just (Text.AthleteInput (AthleteInput.Inputting text cnst)) ->
            swapCurrent (Text.AthleteInput (AthleteInput.Wrong text)) ticker

        _ ->
            ticker



-- INTERNAL


checkAdvanceQueue : Ticker -> Ticker
checkAdvanceQueue ticker =
    case current ticker of
        Nothing ->
            advanceQueue ticker

        Just (Text.Announcement ta) ->
            case ta of
                Text.FinishedAnnouncement _ ->
                    advanceQueue ticker

                Text.InterruptedAnnouncement _ _ ->
                    advanceQueue ticker

                Text.TickingAnnouncement _ _ ->
                    ticker

        Just (Text.Instruction ti) ->
            case ti of
                Text.FinishedInstruction _ ->
                    advanceQueue ticker

                Text.TickingInstruction _ _ ->
                    ticker

        Just (Text.AthleteInput tai) ->
            case tai of
                AthleteInput.Correct _ ->
                    advanceQueue ticker

                AthleteInput.Wrong _ ->
                    advanceQueue ticker

                AthleteInput.Inputting _ _ ->
                    ticker


advanceQueue : Ticker -> Ticker
advanceQueue ticker =
    case ticker of
        ActiveTicker list cur queue ->
            case List.head queue of
                Just new ->
                    ActiveTicker (cur :: list) (fromQueued new) (List.tail queue |> Maybe.withDefault [])

                Nothing ->
                    IdleTicker (cur :: list)

        IdleTicker _ ->
            ticker


fromQueued : Queued -> Text
fromQueued qt =
    case qt of
        Queued.Announcement text ->
            Text.Announcement (Text.TickingAnnouncement text 0)

        Queued.Instruction text ->
            Text.Instruction (Text.TickingInstruction text 0)

        Queued.AthleteInput ->
            Text.AthleteInput (AthleteInput.Inputting "" (Constraints.Serve { initial = 's' }))


swapCurrent : Text -> Ticker -> Ticker
swapCurrent tt ticker =
    case ticker of
        ActiveTicker list cur queue ->
            ActiveTicker list tt queue

        IdleTicker _ ->
            ticker
