module Froggy.State where

import Array
import Maybe
import Time exposing (..)
import Froggy.Util exposing (..)
import Froggy.Grid as Grid
import Froggy.Grid exposing (..)
import Froggy.TransitionUtil exposing (..)
import Froggy.Levels exposing (..)
import Froggy.Model exposing (..)
import Froggy.Commands exposing (..)


game : Signal (Maybe Game) -> Signal Game
game loadedGame = Signal.foldp update initialGame (commands loadedGame)

update : (Time, Command) -> Game -> Game
update (time, command) game =
  case command of
    Nop -> game
    MoveBy positionDelta -> game |> moveBy positionDelta time
    MoveTo leaf -> game |> moveTo leaf time
    Continue -> game |> continue time
    RestartLevel -> game |> restartLevel time
    Start loadedGame -> start loadedGame time

useKeyboard : Bool -> Game -> Game
useKeyboard keyboard game = { game | usingKeyboard = keyboard }

moveBy : Grid.Position -> Time -> Game -> Game
moveBy positionDelta time game =
  let newGame =
    if (positionDelta.x == 0) && (positionDelta.y == 0) then game
    else
      let leafPosition = game.scene.frog.leaf.position `translate` positionDelta
          maybeLeaf = game.scene.leaves |> findLeaf leafPosition
      in case maybeLeaf of
        Nothing -> game
        Just leaf -> game |> moveTo leaf time
  in newGame |> useKeyboard True

findLeaf : Grid.Position -> List Leaf -> Maybe Leaf
findLeaf position leaves =
  let hasPosition leaf = leaf.position `equals` position
  in leaves |> List.filter hasPosition |> Array.fromList |> Array.get 0

moveTo : Leaf -> Time -> Game -> Game
moveTo leaf time game =
  let reachable = leaf |> reachableBy game.scene.frog
      scene = game.scene
      newGame =
        if reachable then
          { game |
            scene = { scene |
              frog = {
                leaf = leaf,
                lastMove = Just {
                  oldValue = game.scene.frog.leaf,
                  startTime = time
                }
              },
              leaves = remove game.scene.leaves game.scene.frog.leaf
            }
          }
        else game
  in newGame |> useKeyboard False

loadLevel : Time -> Maybe Scene -> Int -> Game
loadLevel time oldScene levelNumber =
  let actualLevelNumber = if (levelNumber >= numberOfLevels) || (levelNumber < 0) then 0 else levelNumber
      level = getLevel actualLevelNumber
      leaves = loadLeafMatrix level.leafMatrix
      maybeLeaf = leaves |> findLeaf level.frogPosition
      leaf = maybeLeaf |> Maybe.withDefault  ( (leaves |> List.head) 
                       |> Maybe.withDefault  { position = { x = 0, y = 0 } } )
      --leaf = maybeLeaf |> getOrElse (leaves |> List.head)

  in {
    scene = {
      frog = {
        leaf = leaf,
        lastMove = Nothing
      },
      leaves = leaves,
      levelNumber = actualLevelNumber
    },
    usingKeyboard = False,
    lastSceneChange = Just {
      oldValue = oldScene,
      startTime = time
    }
  }

loadLeafMatrix : LeafMatrix -> List Leaf
loadLeafMatrix leafMatrix = leafMatrix |> List.indexedMap loadLeafRow |> List.concat

loadLeafRow : Int -> List Bool -> List Leaf
loadLeafRow y row =
  let make x value = {
        position = {
          x = x,
          y = y
        },
        value = value
      }
      isThere { value } = value
      removeValue leaf = { position = { x = leaf.position.x, y = leaf.position.y } }
      --removeValue leaf = { leaf - value }      
  in row |> List.indexedMap make |> List.filter isThere |> List.map removeValue

continue : Time -> Game -> Game
continue time game =
  if       ( game.scene |> levelCompleted ) then game |> nextLevel time
   else if ( game.scene |> stuck )          then game |> restartLevel time
   else game

nextLevel : Time -> Game -> Game
nextLevel time game = loadLevel time (Just game.scene) (game.scene.levelNumber + 1)

restartLevel : Time -> Game -> Game
restartLevel time game = loadLevel time (Just game.scene) game.scene.levelNumber

initialGame : Game
initialGame = newGame 0 Nothing

newGame : Time -> Maybe (TransitionInfo (Maybe Scene)) -> Game
newGame time lastSceneChange =
  let level0 = loadLevel time Nothing 0
  in { level0 |
    lastSceneChange = lastSceneChange
  }

start : Maybe Game -> Time -> Game
start loadedGame time =
  let lastSceneChange = Just {
        oldValue = Nothing,
        startTime = time
      }
  in case loadedGame of
    Nothing -> newGame time lastSceneChange
    Just startedGame -> { startedGame |
      lastSceneChange = lastSceneChange
    }