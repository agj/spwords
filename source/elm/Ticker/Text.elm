module Ticker.Text exposing (..)

import Ticker.Text.Constraints as Constraints exposing (Constraints)


type Text
    = InterruptedAnnouncement String Int
    | FinishedAnnouncement String
    | Instruction String
    | CorrectAthleteInput String
    | WrongAthleteInput String


type Active
    = ActiveAnnouncement String Int
    | ActiveInstruction String Int
    | ActiveAthleteInput String Constraints


type Queued
    = QueuedAnnouncement String
    | QueuedInstruction String
    | QueuedAthleteInput Constraints
