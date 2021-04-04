module Ticker.Announcement exposing
    ( Announcement
    , create
    , getCurrent
    , isFinished
    , tick
    , toMessage
    )

import Doc.Paragraph as Paragraph exposing (Paragraph)
import Doc.Util
import Ticker.Message as Message exposing (Message)


type Announcement
    = Announcement ( Paragraph, Int )


create : Paragraph -> Announcement
create par =
    Announcement ( par, 0 )



-- ACCESSORS


getCurrent : Announcement -> Paragraph
getCurrent (Announcement ( par, ticks )) =
    Doc.Util.paragraphLeft ticks par


isFinished : Announcement -> Bool
isFinished (Announcement ( par, ticks )) =
    ticks >= Paragraph.length par



-- MODIFICATION


tick : Announcement -> Announcement
tick (Announcement ( par, ticks )) =
    Announcement ( par, ticks + 1 )


toMessage : Announcement -> Message
toMessage ((Announcement ( par, ticks )) as ann) =
    if isFinished ann then
        Message.FinishedAnnouncement par

    else
        Message.InterruptedAnnouncement par ticks
