module Announcement exposing
    ( Announcement
    , create
    , tick
    )

import Doc.Paragraph as Paragraph exposing (Paragraph)


type Announcement
    = Announcement ( Paragraph, Int )


create : Paragraph -> Announcement
create par =
    Announcement ( par, 0 )



-- MODIFICATION


tick : Announcement -> Announcement
tick (Announcement ( par, ticks )) =
    Announcement ( par, ticks + 1 )
