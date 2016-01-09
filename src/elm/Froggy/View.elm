module Froggy.View where

import Maybe
import Maybe.Extra exposing (isJust)
import Text
import Color exposing (..)
import Time exposing (..)
import Graphics.Element as Element exposing (..)
import Graphics.Input exposing (..)
import Graphics.Collage exposing (..)
import Easing
import Easing exposing (..)
import Froggy.Util exposing (..)
import Froggy.TransitionUtil exposing (..)
import Froggy.Grid as Grid
import Froggy.Grid exposing (..)
import Froggy.Levels exposing (..)
import Froggy.Model exposing (..)
import Froggy.Commands exposing (..)

view : String -> (Int, Int) -> Time -> Game -> Element
view fontName (windowWidth, windowHeight) time game =
  case game.lastSceneChange of
    Just lastSceneChange ->
      let viewSize = min windowWidth windowHeight
          scene = game.scene |> viewScene fontName viewSize time lastSceneChange
          keyboardHint = game |> viewKeyboardHint fontName viewSize time |> collage viewSize viewSize
          cover = lastSceneChange |> viewCover (windowWidth, windowHeight) time
      in [scene, keyboardHint, cover] |> List.map (container windowWidth windowHeight middle) |> layers
    Nothing ->
      let blackRectangle = spacer windowWidth windowHeight |> Element.color black
          loadingImage = image 64 64 (imagePath "loading.gif") |> container windowWidth windowHeight middle
      in layers [blackRectangle, loadingImage]

viewScene : String -> Int -> Time -> TransitionInfo (Maybe Scene) -> Scene -> Element
viewScene fontName viewSize time lastSceneChange scene = 
  let actualScene = case lastSceneChange.oldValue of
        Just oldScene -> if (time - lastSceneChange.startTime) < (sceneChangeDuration / 2) then oldScene else scene
        Nothing -> scene
      tileSize = viewSize |> getTileSize
      frog = actualScene.frog |> viewFrog tileSize time
      leaves = actualScene.leaves |> List.map (viewLeaf tileSize)
      targets = actualScene.leaves |> viewTargets actualScene.frog tileSize
      level = actualScene.levelNumber |> viewLevelNumber fontName tileSize
      message = actualScene |> viewMessage fontName tileSize time
  in (leaves ++ targets ++ frog ++ level ++ message) |> collage viewSize viewSize

getTileSize : Int -> Float
getTileSize viewSize = (viewSize |> toFloat) / mapSize

mapSize = 8

viewFrog : Float -> Time -> Frog -> List Form
viewFrog tileSize time frog =
  let newWorldPosition = frog.leaf.position |> toWorld tileSize
      worldPosition = case frog.lastMove of
        Just { oldValue, startTime } ->
          let oldWorldPosition = oldValue.position |> toWorld tileSize
          in ease easeInOutQuint (pair float) oldWorldPosition newWorldPosition moveDuration (time - startTime)
        Nothing -> newWorldPosition
      lastLeaf = case frog.lastMove of
        Nothing -> []
        Just { oldValue, startTime } ->
          let alphaValue = ease easeInCubic float 1 0 moveDuration (time - startTime)
          in [viewLeaf tileSize oldValue |> alpha alphaValue]
      size = case frog.lastMove of
        Nothing -> 1
        Just { startTime } -> ease (easeInQuad |> retour) float 1 1.2 moveDuration (time - startTime)
      frogSprite = sprite worldPosition tileSize (imagePath "frog.png") |> rotate (angleOf frog |> toFloat |> degrees) |> scale size
  in lastLeaf ++ [frogSprite]

moveDuration : Time
moveDuration = 250 * millisecond

sprite : (Float, Float) -> Float -> String -> Form
sprite = customSprite identity

customSprite : (Element -> Element) -> (Float, Float) -> Float -> String -> Form
customSprite transform worldPosition tileSize url =
  let element = image (round tileSize) (round tileSize) url
  in element |> transform |> makeForm worldPosition

makeForm : (Float, Float) -> Element -> Form
makeForm worldPosition element = element |> toForm |> move worldPosition

toWorld : Float -> Grid.Position -> (Float, Float)
toWorld tileSize position =
  let transform coordinate = ((coordinate |> toFloat) - mapSize / 2 + 0.5) * tileSize
  in (transform position.x, -(transform position.y))

viewLeaf : Float -> Leaf -> Form
viewLeaf tileSize leaf =
  let worldPosition = leaf.position |> toWorld tileSize
  in sprite worldPosition tileSize (imagePath "leaf.png")

viewTargets : Frog -> Float -> List Leaf -> List Form
viewTargets frog tileSize leaves =
  let targets              = leaves |> List.filter (reachableBy frog)
      toClickable target   = clickable ( Signal.message moveTo.address (MoveTo target) )
      distanceOf target    = ( distance target.position.x frog.leaf.position.x ) + ( distance target.position.y frog.leaf.position.y )
      angleOf target       = frog.leaf `angleBetween` target |> getOrElse 0
      filename target      = "arrows/" ++ (distanceOf target |> toString ) ++ "/" ++ ( angleOf target |> toString  ) ++ ".svg" |> imagePath
      worldPosition target = target.position |> toWorld tileSize
      viewTarget target    = customSprite (toClickable target) (worldPosition target) tileSize (filename target)
  in targets |> List.map viewTarget

viewLevelNumber : String -> Float -> Int -> List Form
viewLevelNumber fontName tileSize levelNumber =
  let position = (getLevel levelNumber) |> .levelPosition
      worldPosition = position |> toWorld tileSize
      background = sprite worldPosition tileSize (imagePath "level.png")
      indicator = textSprite fontName position tileSize ("Level\n" ++ toString levelNumber ++ "/" ++ toString  (numberOfLevels - 1)) |> rotate (-1 |> degrees)
  in [background, indicator]

textSprite : String -> Grid.Position -> Float -> String -> Form
textSprite fontName position tileSize string =
  let textSize = tileSize / 5
      worldPosition = position |> toWorld tileSize
  in gameText fontName textSize string |> makeForm worldPosition

gameText : String -> Float -> String -> Element
gameText fontName height string = Text.fromString string |> Text.style {
    typeface = [fontName],
    height = Just height,
    color = red,
    bold = True,
    italic = False,
    line = Nothing
  } |> centered

viewMessage : String -> Float -> Time -> Scene -> List Form
viewMessage fontName tileSize time scene =
  let inverseAngle = ((angleOf scene.frog + 180) % 360) |> toFloat
      deltaX = cos (inverseAngle |> degrees) |> round
      deltaY = (-1 * sin (inverseAngle |> degrees)) |> round
      position = scene.frog.leaf.position `translate` { x = deltaX, y = deltaY }
      worldPosition = toWorld tileSize position
      filename = "message/" ++ (inverseAngle |> toString ) ++ ".svg" |> imagePath
      background = sprite worldPosition tileSize filename
      iconSize = tileSize / 2.5
      forms =
        if ( scene |> levelCompleted ) then
             let lastLevel = scene.levelNumber == numberOfLevels - 1
             in if lastLevel then
                  [background, sprite worldPosition iconSize (imagePath "completed.svg")]
                else
                  [background, sprite worldPosition iconSize (imagePath "next.png")]
          else if ( scene |> stuck ) then 
            [background, sprite worldPosition iconSize (imagePath "restart.png")]
          else  []
      scaleFactor = case scene.frog.lastMove of
        Just { startTime } -> ease easeInOutBack float 0 1 (moveDuration * 2) (time - startTime)
        Nothing -> 1
  in forms |> List.map (scale scaleFactor)

imagePath : String -> String
imagePath filename = "images/" ++ filename

viewKeyboardHint : String -> Int -> Time -> Game -> List Form
viewKeyboardHint fontName viewSize time game =
  if game.usingKeyboard then
    let tileSize = viewSize |> getTileSize
        gridPosition = game.scene.levelNumber |> getLevel |> .keyboardHintPosition
        worldPosition = gridPosition |> toWorld tileSize
        background = sprite worldPosition tileSize (imagePath "key.svg")
        text string = string |> textSprite fontName gridPosition tileSize 
        forms = 
          if      ( (game.scene |> levelCompleted) && (game.scene.levelNumber == 0) ) then [background, text "Enter"]
          else if ( (game.scene |> stuck) && not (game.scene |> levelCompleted) )     then [background, text "Esc"]
          else if ( (game.scene |> onlyDoubleJump) && (game.scene.levelNumber == 0) ) then [background, text "Shift"]
          else  []
        scaleFactor = case game.scene.frog.lastMove of
          Just { startTime } -> ease easeInOutQuad float 0 1 moveDuration (time - startTime)
          Nothing -> 1
    in forms |> List.map (scale scaleFactor)
  else []

viewCover : (Int, Int) -> Time -> TransitionInfo (Maybe Scene) -> Element
viewCover (windowWidth, windowHeight) time lastSceneChange =
  let easingFunction = if lastSceneChange.oldValue |> isJust then retour Easing.linear else Easing.flip Easing.linear
      factor = ease easingFunction float 0 1 sceneChangeDuration (time - lastSceneChange.startTime)
  in spacer windowWidth windowHeight |> Element.color black |> opacity factor

sceneChangeDuration : Time
sceneChangeDuration = 300 * millisecond