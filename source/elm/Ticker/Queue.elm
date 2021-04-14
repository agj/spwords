module Ticker.Queue exposing
    ( Queue
    , peek
    , pop
    , push
    , singleton
    )

import Ticker.Announcement exposing (Announcement)


type Queue
    = Queue Announcement (List Announcement)


singleton : Announcement -> Queue
singleton ann =
    Queue ann []


push : Announcement -> Queue -> Queue
push ann (Queue ann2 list) =
    Queue ann (ann2 :: list)


pop : Queue -> ( Announcement, Maybe Queue )
pop (Queue ann list) =
    case list of
        [] ->
            ( ann, Nothing )

        ann2 :: rest ->
            ( ann, Just (Queue ann2 rest) )


peek : Queue -> Announcement
peek (Queue ann _) =
    ann
