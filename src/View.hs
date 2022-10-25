-- | This module defines how to turn
--   the game state into a picture
module View where

import Graphics.Gloss hiding (display) -- conflicts with Model.Entity.display
import Model

imgWallpaper = "wallpaper.bmp"
type ScreenSize = (Int, Int)

timePos   = Coords (-490, 250)
pausedPos = Coords (-390, 250)

--view :: GameState -> IO Picture
--view gs = pictures <$> viewGameState gs

view :: ScreenSize -> GameState -> IO Picture
view screenSize (GameState player keylist enemies time paused) = pictures <$> sequence (
  viewIOPicture (Coords (0, 0)) imgWallpaper            -- Draw the background
    : display player viewIOPicture                      -- Draw player
    : map (`display` viewIOPicture) enemies             -- Draw enemies
   ++ return (viewStats time paused)                    -- Draw stats
    : return (viewPauseMenuIfPaused paused screenSize)  -- Draw pause menu if paused
    : [] -- '[]' is kept to make the structure above more easy to edit
  )

viewText :: (Show a) => Coords -> Float -> Color -> a -> Picture
viewText (Coords (x, y)) size c = translate x y . scale size size . color c . Text . show

viewIOPicture :: Coords -> FilePath -> IO Picture
viewIOPicture (Coords (x, y)) img = translate x y <$> loadBMP img

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
center w h = Coords (-w / 2, -h / 2)
