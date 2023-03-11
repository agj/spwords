module Ticker.Announcement exposing
    ( Announcement
    , create
    , createUnskippable
    , getCurrent
    , isFinished
    , isSkippable
    , tick
    , toMessage
    )

import Doc.Paragraph as Paragraph exposing (Paragraph)
import Doc.Util
import Ticker.Message as Message exposing (Message)


type Announcement
    = Announcement Paragraph Int Skippability


type Skippability
    = Skippable
    | Unskippabble


create : Paragraph -> Announcement
create par =
    Announcement par 0 Skippable


createUnskippable : Paragraph -> Announcement
createUnskippable par =
    Announcement par 0 Unskippabble



-- ACCESSORS


getCurrent : Announcement -> Paragraph
getCurrent (Announcement par ticks _) =
    Doc.Util.paragraphLeft ticks par


isFinished : Announcement -> Bool
isFinished (Announcement par ticks _) =
    ticks >= Paragraph.length par


isSkippable : Announcement -> Bool
isSkippable (Announcement _ _ skippability) =
    case skippability of
        Skippable ->
            True

        Unskippabble ->
            False



-- MODIFICATION


tick : Announcement -> Announcement
tick (Announcement par ticks skippability) =
    Announcement par (ticks + 1) skippability


toMessage : Announcement -> Message
toMessage ((Announcement par ticks _) as ann) =
    if isFinished ann then
        Message.FinishedAnnouncement par

    else
        Message.InterruptedAnnouncement par ticks
