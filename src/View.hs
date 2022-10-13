-- | This module defines how to turn
--   the game state into a picture
module View where

import Graphics.Gloss
import Model

view :: GameState -> IO Picture
view gs = do
  wallpaper <- loadBMP "C:\\Users\\silas\\Dropbox\\Uni\\Functioneel Programmeren\\Game\\src\\wallpaper.bmp"
  return . pictures $ wallpaper : [viewPure gs]

viewPure :: GameState -> Picture
-- viewPure gstate = case infoToShow gstate of
--   ShowNothing   -> blank
--   ShowANumber n -> color green (text (show n))
--   ShowAChar   c -> color green (text [c])
viewPure gameState = circleSolid 800