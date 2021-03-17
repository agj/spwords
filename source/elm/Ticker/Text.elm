module Ticker.Text exposing (..)


type Text
    = Announcement Announcement
    | AthleteInput AthleteInput


type Announcement
    = TickingAnnouncement String Int
    | InterruptedAnnouncement String Int
    | FinishedAnnouncement String


type AthleteInput
    = InputtingAthleteInput String
