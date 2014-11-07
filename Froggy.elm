module Froggy where

import Array
import Keyboard
import Window
import Maybe

-- Model

data GameState =
  GameOver |
  Playing Game

type Game = {
  frog: Frog,
  leaves: [Leaf],
  levelNumber: Int
}

type Frog = {
  leaf: Leaf,
  direction: Direction
}

data Direction = Up | Right | Down | Left

type Leaf = {
  position: Position
}

type Position = {
  x: Int,
  y: Int
}

defaultGameState = loadLevel 0

gameState : Signal GameState
gameState = foldp update defaultGameState commands

--- Levels

type Level = {
  position: Position,
  leafMatrix: LeafMatrix
}

type LeafMatrix = [[Bool]]

levels =
  Array.fromList [
    {
      position = {
        x = 4,
        y = 1
      },
      leafMatrix = [
        [False, False, False, False, False, False, False],
        [False, False, False, False, True , False, False],
        [False, False, False, False, True , False, False],
        [False, True , True , True , True , True , True ],
        [False, True , False, False, True , False, False],
        [False, True , False, False, True , False, False],
        [False, True , True , True , True , False, False]
      ]
    }
  ]

-- Commands

data Command =
  MoveBy Position |
  MoveTo Leaf |
  Restart

update : Command -> GameState -> GameState
update command gameState =
  case command of
    MoveBy direction ->
      case gameState of
        GameOver -> gameState
        Playing game -> Playing { game | frog <- game.frog |> moveBy direction game.leaves }
    MoveTo leaf ->
      case gameState of
        GameOver -> gameState
        Playing game -> Playing { game | frog <- game.frog |> moveTo leaf }
    Restart ->
      case gameState of
        GameOver -> loadLevel 0
        Playing game -> loadLevel game.levelNumber

moveBy : Position -> [Leaf] -> Frog -> Frog
moveBy direction leaves frog =
  if (direction.x == 0) && (direction.y == 0)
  then
    frog
  else
    let leafPosition = frog.leaf.position `translate` direction
        maybeLeaf = leaves |> findLeaf leafPosition
    in case maybeLeaf of
      Nothing -> frog
      Just leaf -> frog |> moveTo leaf

translate : Position -> Position -> Position
translate a b =
  {
    x = a.x + b.x,
    y = a.y + b.y
  }

findLeaf : Position -> [Leaf] -> Maybe Leaf
findLeaf position leaves =
  let hasPosition leaf = leaf.position `equals` position
  in leaves |> filter hasPosition |> Array.fromList |> Array.get 0

equals : Position -> Position -> Bool
equals a b = (a.x == b.x) && (a.y == b.y)

moveTo : Leaf -> Frog -> Frog
moveTo leaf frog =
  let maybeDirection = frog |> directionTo leaf
  in case maybeDirection of
    Nothing -> frog
    Just direction ->
      { frog |
        leaf <- leaf,
        direction <- direction
      }

directionTo : Leaf -> Frog -> Maybe Direction
directionTo leaf frog =
  let frogX = frog.leaf.position.x
      frogY = frog.leaf.position.y
      leafX = leaf.position.x
      leafY = leaf.position.y
  in if | (frogX == leafX) && (frogY > leafY) && (frogY `near` leafY) -> Just Up
        | (frogY == leafY) && (frogX < leafX) && (frogX `near` leafX) -> Just Right
        | (frogX == leafX) && (frogY < leafY) && (frogY `near` leafY) -> Just Down
        | (frogY == leafY) && (frogX > leafX) && (frogX `near` leafX) -> Just Left
        | otherwise -> Nothing

near : Int -> Int -> Bool
near a b = abs(a - b) <= maxDistance

maxDistance = 2

loadLevel : Int -> GameState
loadLevel levelNumber =
  let maybeLevel = levels |> Array.get levelNumber
  in case maybeLevel of
    Nothing -> GameOver
    Just level -> Playing (level |> toGame levelNumber)

toGame : Int -> Level -> Game
toGame levelNumber level =
  let leaves = loadLeafMatrix level.leafMatrix
      maybeLeaf = leaves |> findLeaf level.position
      theLeaf = maybeLeaf |> getOrElse (leaves |> head)
  in {
    frog = {
      leaf = theLeaf,
      direction = Right
    },
    levelNumber = levelNumber,
    leaves = leaves
  }

loadLeafMatrix : LeafMatrix -> [Leaf]
loadLeafMatrix leafMatrix = leafMatrix |> indexedMap loadLeafRow |> concat

loadLeafRow : Int -> [Bool] -> [Leaf]
loadLeafRow y row =
  let make x value = {
        position = {
          x = x,
          y = y
        },
        value = value
      }
      isThere { value } = value
      removeValue leaf = { leaf - value }
  in row |> indexedMap make |> filter isThere |> map removeValue

getOrElse : a -> Maybe a -> a
getOrElse defaultValue maybeValue = maybeValue |> Maybe.maybe defaultValue identity

-- Input

direction : Signal Position
direction = lift2 makeDirection Keyboard.shift Keyboard.arrows

makeDirection : Bool -> Position -> Position
makeDirection shift arrows =
  let multiplier = if shift then 2 else 1
  in {
    x = arrows.x * multiplier,
    y = -arrows.y * multiplier
  }

commands : Signal Command
commands = lift MoveBy direction

-- View

main = lift2 view Window.dimensions gameState

view : (Int, Int) -> GameState -> Element
view (w, h) gameState =
  let background = fittedImage w h "http://lh5.ggpht.com/-pc0Bk49G7Cs/T5RYCdQjj1I/AAAAAAAAAmQ/e494iWINcrI/s9000/Texture%252Bacqua%252Bpiscina%252Bwater%252Bpool%252Bsimo-3d.jpg"
      viewSize = min w h
      tileSize = (viewSize |> toFloat) / mapSize
      foreground = (viewGameState tileSize gameState) |> collage viewSize viewSize |> container w h middle
  in layers [background, foreground]

mapSize = 8

viewGameState : Float -> GameState -> [Form]
viewGameState tileSize gameState = case gameState of
  GameOver -> [plainText "Game Over" |> toForm]
  Playing game ->
    let frog = viewFrog tileSize game.frog
        leaves = game.leaves |> map (viewLeaf tileSize)
    in leaves ++ [frog]

viewFrog : Float -> Frog -> Form
viewFrog tileSize frog =
  let angle = case frog.direction of
        Up -> 0
        Right -> -90
        Down -> 180
        Left -> 90
  in sprite frog.leaf.position tileSize "https://az31353.vo.msecnd.net/pub/enuofhjd" |> rotate (angle |> degrees)

viewLeaf : Float -> Leaf -> Form
viewLeaf tileSize leaf = sprite leaf.position tileSize "https://az31353.vo.msecnd.net/pub/ebfvplpg"

sprite : Position -> Float -> String -> Form
sprite position tileSize url =
  let element = image (floor tileSize) (floor tileSize) url
      worldPosition = position |> toWorld tileSize
  in element |> toForm |> move worldPosition

toWorld : Float -> Position -> (Float, Float)
toWorld tileSize position =
  let transform coordinate = ((coordinate |> toFloat) - mapSize / 2 + 0.5) * tileSize
  in (transform position.x, -(transform position.y))
