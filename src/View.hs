{-#LANGUAGE DuplicateRecordFields#-}

-- | This module defines how to turn
--   the game state into a picture
module View where

import Graphics.Gloss hiding (display) -- conflicts with Model.Entity.display
import Model

type ScreenSize = (Int, Int)

timePos   = Coords (-490) 250
pausedPos = Coords (-280) 250

--view :: GameState -> IO Picture
--view gs = pictures <$> viewGameState gs

view :: ScreenSize -> Picture -> Picture -> Picture -> GameState -> Picture
view screenSize wallpaperImg playerImg alienImg (GameState Player { pos = pos } keylist enemies time paused) = pictures (
  wallpaperImg                                 -- Draw the background
    : translate (x pos) (y pos) playerImg              -- Draw player
    : map (translate  alienImg) enemies      -- Draw enemies
   ++ viewStats time paused                    -- Draw stats
    : viewPauseMenuIfPaused paused screenSize  -- Draw pause menu if paused
    : []
  )

viewText :: (Show a) => Coords -> Float -> Color -> a -> Picture
viewText coords size c = translate (x coords) (y coords) . scale size size . color c . Text . show

viewStats :: Time -> Paused -> Picture
viewStats t paused = pictures [
  viewText timePos 0.3 white t,
  viewText pausedPos 0.3 white paused]

viewPauseMenuIfPaused :: Paused -> ScreenSize -> Picture
viewPauseMenuIfPaused False _      = Blank
viewPauseMenuIfPaused _     (w, h) = pictures [
  color (makeColor 0 0 0 0.6) $ rectangleSolid (fromIntegral w) (fromIntegral h), -- Darken screen
  viewText (center (fromIntegral w / 2) 100) 1 white "paused"]

center :: Float -> Float -> Coords
center w h = Coords (-w / 2) (-h / 2)
