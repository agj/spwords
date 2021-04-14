module Ticker.Active exposing
    ( Active(..)
    , athleteInput
    , fromAnnouncement
    , fromQueue
    )

import Athlete exposing (Athlete)
import Ticker.Announcement exposing (Announcement)
import Ticker.Queue as Queue exposing (Queue)


type Active
    = AthleteInput Athlete String
    | Announcement Announcement


athleteInput : Athlete -> String -> Active
athleteInput athlete input =
    AthleteInput athlete input


fromAnnouncement : Announcement -> Active
fromAnnouncement ann =
    Announcement ann


fromQueue : Queue -> Active
fromQueue queue =
    Queue.peek queue |> Announcement
