module Ticker.Text exposing (..)

import Ticker.Text.AthleteInput as AthleteInput exposing (AthleteInput)


type Text
    = Announcement Announcement
    | Instruction Instruction
    | AthleteInput AthleteInput


type Announcement
    = TickingAnnouncement String Int
    | InterruptedAnnouncement String Int
    | FinishedAnnouncement String


type Instruction
    = TickingInstruction String Int
    | FinishedInstruction String
