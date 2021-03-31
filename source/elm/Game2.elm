module Game2 exposing (..)

-- type alias Game =
--   { type : GameType
--   , turn : Turn
--   , score : Score
--   }
-- type GameType = Hotseat | Single
-- type Turn
--   = Starting
--   | Turn Athlete
--   | TurnEnded Athlete Constraints.InputCheck
-- type Score
--   = PlayingScore Points Points
--   | Winner Athlete Points
-- type Points = Love | One | Two
-- type Messenger
--   = IdleMessenger
--   | GameMessenger Game
------------------------------------------------------
-- loading | ready | playing
-- idle | instruction | announcement | input | cpuInput
-- none | hotseat | single
-- none | score
-- passedTexts


type Status
    = Loading Announcement
    | Ready Passed Announcement
    | Playing Passed Game


type alias Passed =
    List Message


type Game
    = Hotseat Turn
    | Single Turn


type Turn
    = GameStart Announcement
    | Rules Announcement
    | TurnStart PlayingScore Athlete Constraints Announcement
    | Play PlayingScore Athlete String Constraints
    | PlayCorrect PlayingScore Athlete Announcement
    | PlayWrong Score Athlete ReasonWrong Announcement
    | RoundEnd Score Announcement
    | NewRound PlayingScore Announcement
    | Tally Score Announcement
    | End Athlete Points Announcement


type alias Announcement =
    ( Paragraph, Int )


type ReasonWrong
    = InitialWrong
    | IncorporatesWrong
    | AlreadyPlayed
    | NotAWord


type Score
    = PlayingScore PlayingScore
    | WinnerScore Athlete Points


type alias PlayingScore =
    ( Points, Points )


type Points
    = Love
    | One
    | Two
