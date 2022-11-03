{-#LANGUAGE DuplicateRecordFields#-}

-- | This module defines how to turn
--   the game state into a picture
module View where

import Graphics.Gloss hiding (display) -- conflicts with Model.Entity.display
import Model hiding (rotate)
import Data.Maybe
import SupportiveFunctions

type ScreenSize = (Int, Int)

timePos   = Coords (-490) 250
pausedPos = Coords (-280) 250

view :: ScreenSize -> [(String, Picture)] -> GameState -> Picture
view screenSize textures (GameState Player { pos = playerPos } keylist enemies time paused alive rng) = pictures (
 getTexture "wallpaper"                        -- Draw the background
  : translate' playerPos (getTexture "player") -- Draw player
  : map viewEnemy enemies                      -- Draw enemies
 ++ viewStats time paused                      -- Draw stats
  : viewPauseMenuIfPaused paused screenSize    -- Draw pause menu if paused
  : viewGameOverIfPlayerDead alive screenSize
  : []
  )
  where
    translate' coords = translate (x coords) (y coords)
    getTexture        = fromJust . flip lookup textures
    viewEnemy e       = viewGeneric (getPos e) (getSize e) (getRotation e) (getTexture $ imgKey e)

viewGeneric :: Coords -> Size -> Rotation -> Picture -> Picture
viewGeneric (Coords x y) size rotation = translate x y . rotate rotation . scale size size

viewText :: Coords -> Float -> Color -> String -> Picture
viewText coords size c = color c . viewGeneric coords size 0 . Text

viewStats :: Time -> Paused -> Picture
viewStats t paused = pictures [
  viewText timePos 0.3 white $ show (roundToDecimals t 2),
  viewText pausedPos 0.3 white $ show paused]

viewPauseMenuIfPaused :: Paused -> ScreenSize -> Picture
viewPauseMenuIfPaused False _      = Blank
viewPauseMenuIfPaused _     (w, h) = pictures [
  color (makeColor 0 0 0 0.6) $ rectangleSolid (fromIntegral w) (fromIntegral h), -- Darken screen
  viewText (center (fromIntegral w / 2) 100) 1 white "Paused"]
  
viewGameOverIfPlayerDead :: Alive -> ScreenSize -> Picture
viewGameOverIfPlayerDead True _      = Blank
viewGameOverIfPlayerDead _    (w, h) = pictures [
  color (makeColor 0 0 0 0.6) $ rectangleSolid (fromIntegral w) (fromIntegral h), -- Darken screen
  viewText (center 750 100) 1 white "Game Over!"]

center :: Float -> Float -> Coords
center w h = Coords (-w / 2) (-h / 2)
