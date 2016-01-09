module Froggy.Main where

import Window
import Froggy.Model exposing (..)
import Froggy.State exposing (..)
import Froggy.View exposing (..)
import Froggy.TransitionUtil exposing (..)

main = Signal.map3 (view fontName) Window.dimensions time mainState

mainState = game loadedGame

port loadedGame : Signal (Maybe Game)

port savedGame : Signal Game
port savedGame = mainState

port fontName : String