module Announcement exposing
    ( Announcement
    , create
    , getCurrent
    , tick
    , toMessage
    )

import Doc.Paragraph as Paragraph exposing (Paragraph)
import Doc.Util
import Message exposing (Message)


type Announcement
    = Announcement ( Paragraph, Int )


create : Paragraph -> Announcement
create par =
    Announcement ( par, 0 )



-- ACCESSORS


getCurrent : Announcement -> Paragraph
getCurrent (Announcement ( par, ticks )) =
    Doc.Util.paragraphLeft ticks par



-- MODIFICATION


tick : Announcement -> Announcement
tick (Announcement ( par, ticks )) =
    Announcement ( par, ticks + 1 )


toMessage : Announcement -> Message
toMessage (Announcement ( par, ticks )) =
    if ticks >= Paragraph.length par then
        Message.FinishedAnnouncement par

    else
        Message.InterruptedAnnouncement par ticks
