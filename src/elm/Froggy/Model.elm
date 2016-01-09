module Froggy.Model where

import Maybe exposing (..)
import Maybe.Extra exposing ( isJust )
import Froggy.Util exposing (..)
import Froggy.TransitionUtil exposing (..)
import Froggy.Grid as Grid
import Froggy.Levels exposing (..)

type alias Game = {
  scene: Scene,
  usingKeyboard: Bool,
  lastSceneChange: Maybe (TransitionInfo (Maybe Scene))
}

type alias Scene = {
  frog: Frog,
  leaves: List Leaf,
  levelNumber: Int
}

type alias Frog = {
  leaf: Leaf,
  lastMove: Maybe (TransitionInfo Leaf)
}

type alias Leaf = {
  position: Grid.Position
}

levelCompleted : Scene -> Bool
levelCompleted scene = (scene.leaves |> List.length) == 1

stuck : Scene -> Bool
stuck scene = not (scene.leaves |> List.any (reachableBy scene.frog))

reachableBy : Frog -> Leaf -> Bool
reachableBy frog leaf = (frog.leaf `angleBetween` leaf) |> isJust

angleBetween : Leaf -> Leaf -> Maybe Int
angleBetween sourceLeaf targetLeaf =
  let sourceX = sourceLeaf.position.x
      sourceY = sourceLeaf.position.y
      targetX = targetLeaf.position.x
      targetY = targetLeaf.position.y
  in if (sourceX == targetX) && (sourceY > targetY) && (sourceY `near` targetY)        then Just 90
      else if  (sourceY == targetY) && (sourceX < targetX) && (sourceX `near` targetX) then Just 0
      else if  (sourceX == targetX) && (sourceY < targetY) && (sourceY `near` targetY) then Just 270
      else if  (sourceY == targetY) && (sourceX > targetX) && (sourceX `near` targetX) then Just 180
      else  Nothing

near : Int -> Int -> Bool
near a b = (distance a b) <= 2

onlyDoubleJump : Scene -> Bool
onlyDoubleJump scene =
  let distanceX leaf = distance scene.frog.leaf.position.x leaf.position.x
      distanceY leaf = distance scene.frog.leaf.position.y leaf.position.y
      leafOnlyDoubleJump leaf = ((distanceX leaf) == 2) || ((distanceY leaf) == 2)
  in scene.leaves |> List.filter (reachableBy scene.frog) |> List.all leafOnlyDoubleJump

angleOf : Frog -> Int
angleOf frog =
  case frog.lastMove of
    Just { oldValue } -> oldValue `angleBetween` frog.leaf |> getOrElse 0
    Nothing -> 90