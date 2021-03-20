module Ticker.Text exposing (..)

import Doc.Paragraph exposing (Paragraph)
import Ticker.Text.Constraints as Constraints exposing (Constraints)


type Text
    = InterruptedAnnouncement Paragraph Int
    | FinishedAnnouncement Paragraph
    | Instruction Paragraph
    | CorrectAthleteInput String
    | WrongAthleteInput String


type Active
    = ActiveAnnouncement Paragraph Int
    | ActiveInstruction Paragraph Int
    | ActiveAthleteInput String Constraints


type Queued
    = QueuedAnnouncement Paragraph
    | QueuedInstruction Paragraph
    | QueuedAthleteInput Constraints
