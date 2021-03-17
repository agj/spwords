module Ticker exposing (Announcement(..), AthleteInput(..), Text(..), Ticker)


type alias Ticker =
    List Text


type Text
    = Announcement Announcement
    | AthleteInput AthleteInput


type Announcement
    = TickingTicker String Int
    | InterruptedTicker String Int
    | FinishedTicker String


type AthleteInput
    = InputtingTicker String
