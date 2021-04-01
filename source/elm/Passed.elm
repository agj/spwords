module Passed exposing
    ( Passed
    , empty
    , push
    , pushAnnouncement
    , toList
    )

import Announcement exposing (Announcement)
import Message exposing (Message)


type Passed
    = Passed (List Message)


empty : Passed
empty =
    Passed []


push : Message -> Passed -> Passed
push mes (Passed list) =
    Passed (mes :: list)


pushAnnouncement : Announcement -> Passed -> Passed
pushAnnouncement ann passed =
    push (Announcement.toMessage ann) passed


toList : Passed -> List Message
toList (Passed list) =
    list
