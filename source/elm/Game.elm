module Game exposing
    ( Game
    , active
    , inputted
    , queueUp
    , tick
    )

import Ticker.Text exposing (Active(..), Queued(..))


type Game
    = Hotseat active (List Queued)
    | Idle


active : Game -> Maybe Active
active game =
    case game of
        Hotseat act _ ->
            Just act

        Idle _ ->
            Nothing


inputted : Game -> Maybe String
inputted game =
    case active game of
        Just (ActiveAthleteInput athlete text cnst) ->
            Just text

        _ ->
            Nothing



-- MODIFICATION


queueUp : Queued -> Game -> Game
queueUp q game =
    case game of
        Hotseat act queue ->
            Hotseat act (List.append queue [ q ])

        Idle ->
            Hotseat (fromQueued q) []


tick : Game -> ( Game, Maybe Text )
tick game =
    checkAdvanceQueue <|
        case active game of
            Just (ActiveAnnouncement text ticks) ->
                swapActive
                    (ActiveAnnouncement text (ticks + 1))
                    game

            Just (ActiveInstruction text ticks) ->
                swapActive
                    (ActiveInstruction text (ticks + 1))
                    game

            _ ->
                game


enter : Game -> ( Game, Maybe Text )
enter game =
    case current game of
        Just (ActiveAnnouncement txt ticks) ->
            ( advanceQueue game
            , Just (InterruptedAnnouncement txt ticks)
            )

        _ ->
            ( game, Nothing )


input : String -> Game -> ( Game, Maybe Text )
input text game =
    checkAdvanceQueue <|
        case active game of
            Just (ActiveAthleteInput athlete txt cnst) ->
                let
                    fixedText =
                        text
                            |> String.toLower
                            |> String.filter
                                (\ch ->
                                    String.any ((==) ch) "abcdefghijklmnopqrstuvwxyzñ-'"
                                )
                in
                swapActive
                    (ActiveAthleteInput athlete (txt ++ fixedText) cnst)
                    game

            _ ->
                game


inputCorrect : Game -> ( Game, Maybe Text )
inputCorrect game =
    case active game of
        Just (ActiveAthleteInput athlete text cnst) ->
            ( advanceQueue game
            , Just (CorrectAthleteInput athlete text)
            )

        _ ->
            ( game, Nothing )


inputWrong : Game -> Game
inputWrong game =
    case active game of
        Just (ActiveAthleteInput athlete text cnst) ->
            let
                fixedText =
                    if String.length text == 0 then
                        " "

                    else
                        text
            in
            ( advanceQueue game
            , Just (WrongAthleteInput athlete fixedText)
            )

        _ ->
            ( game, Nothing )



-- INTERNAL


checkAdvanceQueue : Game -> ( Game, Maybe Text )
checkAdvanceQueue game =
    case active game of
        Just (ActiveAnnouncement text ticks) ->
            if ticks >= String.length (Paragraph.toString text) then
                ( advanceQueue game
                , Just (FinishedAnnouncement text)
                )

            else
                ( game, Nothing )

        Just (ActiveInstruction text ticks) ->
            if ticks >= String.length (Paragraph.toString text) then
                ( advanceQueue game
                , Just (Instruction text)
                )

            else
                ( game, Nothing )

        _ ->
            ( game, Nothing )


advanceQueue : Game -> Game
advanceQueue game =
    case game of
        Hotseat _ queue ->
            case List.head queue of
                Just newActive ->
                    Hotseat
                        (fromQueued newActive)
                        (List.tail queue |> Maybe.withDefault [])

                Nothing ->
                    Idle []

        Idle _ ->
            game


fromQueued : Queued -> Active
fromQueued qt =
    case qt of
        QueuedAnnouncement text ->
            ActiveAnnouncement text 0

        QueuedInstruction text ->
            ActiveInstruction text 0

        QueuedAthleteInput athlete cnts ->
            ActiveAthleteInput athlete "" cnts


swapActive : Active -> Game -> Game
swapActive newActive game =
    case game of
        Hotseat _ queue ->
            Hotseat newActive queue

        Idle _ ->
            game
