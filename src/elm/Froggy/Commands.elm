module Froggy.Commands where

import Keyboard
import Mouse
import Time exposing (..)
import Graphics.Input exposing (..)
import Froggy.Grid as Grid
import Froggy.Model exposing (..)

type Command =
  Nop |
  MoveBy Grid.Position |
  MoveTo Leaf |
  Continue |
  RestartLevel |
  Start (Maybe Game)

commands : Signal (Maybe Game) -> Signal (Time, Command)
commands loadedGame =
  let moveBy       = Signal.map2 makeMoveBy Keyboard.shift Keyboard.arrows
      continue     = Signal.map (makeCommand Continue) ( Signal.merge Keyboard.enter Mouse.isDown)
      restartLevel = Signal.map (makeCommand RestartLevel) (Keyboard.isDown 27)
      start        = makeStart loadedGame
  in Signal.mergeMany [moveBy, moveTo.signal, continue, restartLevel, start] |> timestamp

makeMoveBy : Bool -> Grid.Position -> Command
makeMoveBy shift arrows =
  let multiplier = if shift then 2 else 1
  in MoveBy {
    x = arrows.x * multiplier,
    y = -arrows.y * multiplier
  }

--moveTo : Input Command
--moveTo = input Nop

moveTo : Signal.Mailbox Command
moveTo = Signal.mailbox Nop

makeCommand : Command -> Bool -> Command
makeCommand command pressed = if pressed then command else Nop

makeStart : Signal (Maybe Game) -> Signal Command
makeStart loadedGame = Signal.foldp updateStart Nop loadedGame |> Signal.dropRepeats

updateStart : Maybe Game -> Command -> Command
updateStart loadedGame startGame =
  case startGame of
    Nop -> Start loadedGame
    _ -> startGame