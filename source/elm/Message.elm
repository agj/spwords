module Message exposing (..)

import Athlete exposing (..)
import Doc.Paragraph exposing (Paragraph)


type Message
    = InterruptedAnnouncement Paragraph Int
    | FinishedAnnouncement Paragraph
    | CorrectAthleteInput Athlete String
    | WrongAthleteInput Athlete String
