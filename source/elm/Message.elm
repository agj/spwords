module Message exposing (..)

import Athlete exposing (..)
import Constraints exposing (Constraints)
import Doc.Paragraph exposing (Paragraph)


type Message
    = InterruptedAnnouncement Paragraph Int
    | FinishedAnnouncement Paragraph
    | Instruction Paragraph
    | CorrectAthleteInput Athlete String
    | WrongAthleteInput Athlete String


type Active
    = ActiveAnnouncement Paragraph Int
    | ActiveInstruction Paragraph Int
    | ActiveAthleteInput Athlete String Constraints


type Queued
    = QueuedAnnouncement Paragraph
    | QueuedInstruction Paragraph
    | QueuedAthleteInput Athlete Constraints
