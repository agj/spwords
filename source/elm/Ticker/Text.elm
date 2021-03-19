module Ticker.Text exposing (..)

import Ticker.Text.AthleteInput as AthleteInput exposing (AthleteInput)
import Ticker.Text.Constraints as Constraints exposing (Constraints)


type Text
    = Announcement Announcement
    | Instruction Instruction
    | AthleteInput AthleteInput


type Active
    = ActiveAnnouncement String Int
    | ActiveInstruction String Int
    | ActiveAthleteInput String Constraints


type Announcement
    = TickingAnnouncement String Int
    | InterruptedAnnouncement String Int
    | FinishedAnnouncement String


type Instruction
    = TickingInstruction String Int
    | FinishedInstruction String
