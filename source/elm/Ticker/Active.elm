module Ticker.Active exposing
    ( Active(..)
    , athleteInput
    , fromAnnouncement
    )

import Athlete exposing (Athlete)
import Ticker.Announcement exposing (Announcement)


type Active
    = AthleteInput Athlete String
    | Announcement Announcement


athleteInput : Athlete -> String -> Active
athleteInput athlete input =
    AthleteInput athlete input


fromAnnouncement : Announcement -> Active
fromAnnouncement ann =
    Announcement ann
