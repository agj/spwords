module Ticker exposing
    ( Ticker
    , current
    , empty
    , enter
    , fromList
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


type Ticker
    = Ticker (List Text) (List Queued)



-- CONSTRUCTORS


empty : Ticker
empty =
    Ticker [] []


fromList : List Text -> Ticker
fromList list =
    Ticker list []



-- GETTERS


current : Ticker -> Maybe Text
current (Ticker list _) =
    List.head list


toList : Ticker -> List Text
toList (Ticker list _) =
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
        Just (Text.AthleteInput (AthleteInput.Inputting text)) ->
            Just text

        _ ->
            Nothing



-- MODIFICATION


queueUp : Queued.Queued -> Ticker -> Ticker
queueUp q ((Ticker list queue) as ticker) =
    Ticker list (List.append queue [ q ])


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
            Just (Text.AthleteInput (AthleteInput.Inputting txt)) ->
                let
                    fixedText =
                        text
                            |> String.toLower
                            |> String.filter
                                (\ch ->
                                    String.any ((==) ch) "abcdefghijklmnopqrstuvwxyzñ-'"
                                )
                in
                swapCurrent (Text.AthleteInput (AthleteInput.Inputting (txt ++ fixedText))) ticker

            _ ->
                ticker


inputWrong : Ticker -> Ticker
inputWrong ticker =
    case current ticker of
        Just (Text.AthleteInput (AthleteInput.Inputting text)) ->
            swapCurrent (Text.AthleteInput (AthleteInput.Wrong text)) ticker

        _ ->
            ticker



-- INTERNAL


checkAdvanceQueue : Ticker -> Ticker
checkAdvanceQueue ((Ticker list queue) as ticker) =
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

                AthleteInput.Inputting _ ->
                    ticker


advanceQueue : Ticker -> Ticker
advanceQueue ((Ticker list queue) as ticker) =
    Ticker
        (case List.head queue of
            Just cur ->
                fromQueued cur :: list

            Nothing ->
                list
        )
        (List.tail queue |> Maybe.withDefault [])


fromQueued : Queued -> Text
fromQueued qt =
    case qt of
        Queued.Announcement text ->
            Text.Announcement (Text.TickingAnnouncement text 0)

        Queued.Instruction text ->
            Text.Instruction (Text.TickingInstruction text 0)

        Queued.AthleteInput ->
            Text.AthleteInput (AthleteInput.Inputting "")


swapCurrent : Text -> Ticker -> Ticker
swapCurrent tt ((Ticker list queue) as ticker) =
    case list of
        head :: rest ->
            Ticker (tt :: rest) queue

        _ ->
            ticker
