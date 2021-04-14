module Ticker.Queue exposing
    ( Queue
    , fromList
    , peek
    , pop
    , push
    , singleton
    , tick
    )

import Ticker.Announcement as Announcement exposing (Announcement)


type Queue
    = Queue Announcement (List Announcement)


singleton : Announcement -> Queue
singleton ann =
    Queue ann []


fromList : Announcement -> List Announcement -> Queue
fromList ann rest =
    Queue ann rest


push : Announcement -> Queue -> Queue
push ann (Queue ann2 rest) =
    Queue ann (ann2 :: rest)


pop : Queue -> ( Announcement, Maybe Queue )
pop (Queue ann rest) =
    case rest of
        [] ->
            ( ann, Nothing )

        ann2 :: anns ->
            ( ann, Just (Queue ann2 anns) )


peek : Queue -> Announcement
peek (Queue ann _) =
    ann


tick : Queue -> Queue
tick (Queue ann rest) =
    Queue (Announcement.tick ann) rest
