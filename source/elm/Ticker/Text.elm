module Ticker.Text exposing (..)


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


type AthleteInput
    = InputtingAthleteInput String
    | CorrectAthleteInput String
    | WrongAthleteInput String
