module Ticker exposing
    ( Ticker
    , current
    , fromList
    , tick
    , ticking
    )

import Ticker.Text as Text exposing (Text)


type alias Ticker =
    List Text



-- CONSTRUCTORS


fromList : List Text -> Ticker
fromList list =
    list



-- GETTERS


ticking : Ticker -> Bool
ticking ticker =
    case ticker of
        (Text.Announcement (Text.TickingAnnouncement _ _)) :: _ ->
            True

        _ ->
            False


current : Ticker -> Maybe Text
current ticker =
    List.head ticker



-- MODIFICATION


tick : Ticker -> Ticker
tick ticker =
    case ticker of
        (Text.Announcement (Text.TickingAnnouncement text ticks)) :: rest ->
            if ticks < String.length text then
                Text.Announcement (Text.TickingAnnouncement text (ticks + 1)) :: rest

            else
                Text.Announcement (Text.FinishedAnnouncement text) :: rest

        _ ->
            ticker
