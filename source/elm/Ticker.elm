module Ticker exposing
    ( Ticker
    , current
    , empty
    , fromList
    , input
    , inputted
    , queueUp
    , tick
    , ticking
    , toList
    )

import Ticker.Queued as Queued exposing (Queued)
import Ticker.Text as Text exposing (Text)


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
        Just (Text.AthleteInput (Text.InputtingAthleteInput text)) ->
            Just text

        _ ->
            Nothing



-- MODIFICATION


queueUp : Queued.Queued -> Ticker -> Ticker
queueUp q ((Ticker list queue) as ticker) =
    Ticker list (List.append queue [ q ])


tick : Ticker -> Ticker
tick ((Ticker list queue) as ticker) =
    checkAdvanceQueue <|
        case list of
            (Text.Announcement (Text.TickingAnnouncement text ticks)) :: rest ->
                if ticks < String.length text then
                    Ticker (Text.Announcement (Text.TickingAnnouncement text (ticks + 1)) :: rest) queue

                else
                    Ticker (Text.Announcement (Text.FinishedAnnouncement text) :: rest) queue

            _ ->
                ticker


input : String -> Ticker -> Ticker
input text ((Ticker list queue) as ticker) =
    checkAdvanceQueue <|
        case list of
            (Text.Announcement (Text.TickingAnnouncement txt ticks)) :: rest ->
                if text == "\n" then
                    Ticker (Text.Announcement (Text.InterruptedAnnouncement txt ticks) :: rest) queue

                else
                    ticker

            (Text.AthleteInput (Text.InputtingAthleteInput txt)) :: rest ->
                let
                    fixedText =
                        text
                            |> String.toUpper
                            |> String.filter
                                (\ch ->
                                    String.any ((==) ch) "ABCDEFGHIJKLMNOPQRSTUVWXYZÑ-'"
                                )
                in
                Ticker (Text.AthleteInput (Text.InputtingAthleteInput (txt ++ fixedText)) :: rest) queue

            _ ->
                ticker



-- INTERNAL


checkAdvanceQueue : Ticker -> Ticker
checkAdvanceQueue ((Ticker list queue) as ticker) =
    case current ticker of
        Just (Text.Announcement (Text.FinishedAnnouncement _)) ->
            advanceQueue ticker

        Just (Text.Announcement (Text.InterruptedAnnouncement _ _)) ->
            advanceQueue ticker

        Nothing ->
            advanceQueue ticker

        _ ->
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

        Queued.AthleteInput ->
            Text.AthleteInput (Text.InputtingAthleteInput "")
