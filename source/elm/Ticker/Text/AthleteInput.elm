module Ticker.Text.AthleteInput exposing (..)

import Ticker.Text.Constraints as Constraints exposing (Constraints)
import Utils
import Words exposing (Words)


temp =
    ()



-- inputIsCandidate : AthleteInput -> Words -> Bool
-- inputIsCandidate ai words =
--     let
--         initial_ =
--             case cnts of
--                 Constraints.Serve { initial } ->
--                     initial
--                 Constraints.Rally { initial } ->
--                     initial
--     in
--     case Utils.stringHead text of
--         Just head ->
--             (head == initial_)
--                 && Words.candidate text words
--         Nothing ->
--             False
