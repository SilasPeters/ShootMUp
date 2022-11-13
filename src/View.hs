{-#LANGUAGE DuplicateRecordFields#-}

-- | This module defines how to turn
--   the game state into a picture
module View where

import Graphics.Gloss
import Model hiding (rotate)
import Data.Maybe
import SupportiveFunctions

type ScreenSize = (Int, Int)

timePos       = Coords (-490) 270
difficultyPos = Coords( -490) 240

view :: ScreenSize -> [(String, Picture)] -> GameState -> Picture
view screenSize textures (GameState Player { pos = playerPos } _ enemies despawningEnemies time paused alive rng _ difficulty) = pictures (
    getTexture "wallpaper"                           -- Draw the background
      : translate' playerPos (getTexture "player")   -- Draw player
      : map viewEnemy (enemies ++ despawningEnemies) -- Draw enemies
    ++ viewStats time difficulty                     -- Draw stats
      : viewPauseMenuIfPaused paused screenSize      -- Draw pause menu if paused
      : viewGameOverIfPlayerDead alive screenSize
      : []
   )
  where
    translate' coords = translate (x coords) (y coords)
    getTexture        = fromJust . flip lookup textures
    viewEnemy e       = viewGeneric (getPos e) (getScale e) (getRotation e) (getTexture $ entityId e)

viewGeneric :: Coords -> Scale -> Rotation -> Picture -> Picture
viewGeneric (Coords x y) size rotation = translate x y . rotate rotation . scale size size

viewText :: Coords -> Float -> Color -> String -> Picture
viewText coords size c = color c . viewGeneric coords size 0 . Text

viewStats :: Time -> Difficulty -> Picture
viewStats t difficulty = pictures [
  viewText timePos       0.2 white $ "Total playtime: " ++ show (roundToDecimals t 2),
  viewText difficultyPos 0.2 white $ "Current difficulty: " ++ show (roundToDecimals difficulty 8)]

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

showTime :: GameState -> String
showTime gs = show (t gs)

center :: Float -> Float -> Coords
center w h = Coords (-w / 2) (-h / 2)
