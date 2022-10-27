module Main where

import Controller
import Model
import View

import Graphics.Gloss.Interface.Pure.Game
import Graphics.Gloss

screenSize = (1000, 600)
initialState = GameState
  (Player (Coords (-350) 0))   -- player
  []                           -- keyList
  [Alien (Coords 300 100) 1 1] -- aliens
  0                            -- elapsed time
  False                        -- paused

imgWallpaper = "wallpaper.bmp" -- Source: self made in paint
imgPlayer    = "spaceship.bmp" -- todo: mention source
imgAlien     = "alien.bmp" -- Source: https://clipartcraft.com/explore/spaceship-clipart-pixel/

main :: IO ()
main = do
  wallpaperImg <- loadBMP imgWallpaper
  playerImg    <- loadBMP imgPlayer
  alienImg     <- loadBMP imgAlien
  -- all IO is loaded, now run the game in a pure environment
  play (InWindow "Shoot'm up" screenSize (0, 0)) -- Or FullScreen
              white              -- Background color
              60                 -- Frames per second
              initialState       -- Initial state
              (view screenSize wallpaperImg playerImg alienImg)  -- View function
              input              -- Event function
              step               -- Step function