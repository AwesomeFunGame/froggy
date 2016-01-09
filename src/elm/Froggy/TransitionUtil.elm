module Froggy.TransitionUtil where

import Time exposing (..)
import Signal 
-- exposing (..)


type alias TransitionInfo a = {
  oldValue: a,
  startTime: Time
}

time : Signal Time
time = fps 30 |> timestamp |> Signal.map fst