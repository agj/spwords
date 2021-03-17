module Ticker exposing
    ( Ticker
    , current
    , fromList
    , input
    , tick
    , ticking
    , toList
    )

import Ticker.Text as Text exposing (Text)


type Ticker
    = Ticker (List Text)



-- CONSTRUCTORS


fromList : List Text -> Ticker
fromList list =
    Ticker list



-- GETTERS


toList : Ticker -> List Text
toList (Ticker list) =
    list


ticking : Ticker -> Bool
ticking ticker =
    case current ticker of
        Just (Text.Announcement (Text.TickingAnnouncement _ _)) ->
            True

        _ ->
            False


current : Ticker -> Maybe Text
current (Ticker list) =
    List.head list



-- MODIFICATION


tick : Ticker -> Ticker
tick ((Ticker list) as ticker) =
    case list of
        (Text.Announcement (Text.TickingAnnouncement text ticks)) :: rest ->
            if ticks < String.length text then
                Ticker (Text.Announcement (Text.TickingAnnouncement text (ticks + 1)) :: rest)

            else
                Ticker (Text.Announcement (Text.FinishedAnnouncement text) :: rest)

        _ ->
            ticker


input : String -> Ticker -> Ticker
input text ((Ticker list) as ticker) =
    case list of
        (Text.Announcement (Text.TickingAnnouncement txt ticks)) :: rest ->
            if text == "\n" then
                Ticker (Text.Announcement (Text.InterruptedAnnouncement txt ticks) :: rest)

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
            Ticker (Text.AthleteInput (Text.InputtingAthleteInput (txt ++ fixedText)) :: rest)

        _ ->
            ticker
