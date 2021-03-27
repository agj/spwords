module Ticker.Text exposing (..)

import Athlete exposing (..)
import Doc.Paragraph exposing (Paragraph)
import Ticker.Text.Constraints as Constraints exposing (Constraints)


type Text
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
