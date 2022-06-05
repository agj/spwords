module Ticker.Message exposing (..)

import Athlete exposing (..)
import Doc.Paragraph as Paragraph exposing (Paragraph)


type Message
    = InterruptedAnnouncement Paragraph Int
    | FinishedAnnouncement Paragraph
    | CorrectAthleteInput Athlete String
    | WrongAthleteInput Athlete String


toString msg =
    case msg of
        InterruptedAnnouncement par _ ->
            Paragraph.toString par

        FinishedAnnouncement par ->
            Paragraph.toString par

        CorrectAthleteInput _ str ->
            str

        WrongAthleteInput _ str ->
            str
