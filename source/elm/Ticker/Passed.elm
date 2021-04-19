module Ticker.Passed exposing
    ( Passed
    , empty
    , fromList
    , push
    , pushAnnouncement
    , singleton
    , toList
    )

import Ticker.Announcement as Announcement exposing (Announcement)
import Ticker.Message as Message exposing (Message)


type Passed
    = Passed (List Message)


empty : Passed
empty =
    Passed []


singleton : Message -> Passed
singleton mes =
    Passed [ mes ]


fromList : List Message -> Passed
fromList list =
    Passed list


push : Message -> Passed -> Passed
push mes (Passed list) =
    Passed (mes :: list)


pushAnnouncement : Announcement -> Passed -> Passed
pushAnnouncement ann passed =
    push (Announcement.toMessage ann) passed


toList : Passed -> List Message
toList (Passed list) =
    list
