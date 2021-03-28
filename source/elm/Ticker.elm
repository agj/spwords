module Ticker exposing
    ( Ticker
    , current
    , empty
    , enter
    , input
    , inputCorrect
    , inputWrong
    , inputted
    , passed
    , queueUp
    , tick
    )

import Game exposing (Game)
import Ticker.Text as Text exposing (Active, Queued, Text)
import Ticker.Text.Constraints as Constraints exposing (Constraints)


type Ticker
    = Ticker (List Text) Game



-- CONSTRUCTORS


empty : Ticker
empty =
    Ticker [] Game.empty



-- GETTERS


current : Ticker -> Maybe Active
current (Ticker _ game) =
    Game.active game


passed : Ticker -> List Text
passed (Ticker p _) =
    p


inputted : Ticker -> Maybe String
inputted (Ticker _ game) =
    Game.inputted game



-- MODIFICATION


queueUp : Queued -> Ticker -> Ticker
queueUp q (Ticker p game) =
    Ticker p (Game.queueUp q game)


tick : Ticker -> Ticker
tick ((Ticker _ game) as ticker) =
    Game.tick game
        |> handleNewText ticker


enter : Ticker -> Ticker
enter ((Ticker _ game) as ticker) =
    Game.enter game
        |> handleNewText ticker


input : String -> Ticker -> Ticker
input text ((Ticker _ game) as ticker) =
    Game.input text game
        |> handleNewText ticker


inputCorrect : Ticker -> Ticker
inputCorrect ((Ticker _ game) as ticker) =
    Game.inputCorrect game
        |> handleNewText ticker


inputWrong : Ticker -> Ticker
inputWrong ((Ticker _ game) as ticker) =
    Game.inputWrong game
        |> handleNewText ticker



-- INTERNAL


handleNewText : Ticker -> ( Game, Maybe Text ) -> Ticker
handleNewText (Ticker p _) ( newGame, textM ) =
    case textM of
        Just text ->
            Ticker (text :: p) newGame

        Nothing ->
            Ticker p newGame
