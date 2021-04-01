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
    | Ready Words Passed Announcement
    | Playing Words Passed Game


type alias Passed =
    List Message


type Game
    = Hotseat Turn
    | Single Turn


type Turn
    = GameStart Announcement
    | Rules Announcement
    | RoundStart PlayingScore Athlete Constraints Announcement
    | Play PlayingScore Athlete String Constraints
    | PlayCorrect Score Athlete Constraints Announcement
    | PlayWrong Score Athlete Constraints Announcement
    | RoundEnd Score Athlete Announcement
    | NewRound PlayingScore Athlete Announcement
    | Tally Score Athlete Announcement
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
