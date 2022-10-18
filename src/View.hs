-- | This module defines how to turn
--   the game state into a picture
module View where

import Graphics.Gloss
import Model

view :: GameState -> IO Picture
view gs@(GameState(Player (CoordY y))) = do
  wallpaper <- loadBMP "wallpaper.bmp"
  player <- loadBMP "spaceship.bmp"
  return $ pictures [wallpaper, translate (-350) y player]

--viewPure :: GameState -> Picture
-- viewPure gstate = case infoToShow gstate of
--   ShowNothing   -> blank
--   ShowANumber n -> color green (text (show n))
--   ShowAChar   c -> color green (text [c])
-- viewPure gameState = circleSolid 800