{-#LANGUAGE DuplicateRecordFields#-}

module Main where

import Controller
import Model
import View

import Graphics.Gloss.Interface.IO.Game
import Graphics.Gloss
import System.Random

screenSize = (1000, 600)
initialState = GameState
  (Player (Coords (-350) 0) (60, 35) 500)   -- player
  []                               -- keyList
  []                               -- Aliens
  []
  0                                -- elapsed time
  False                            -- paused
  True                             -- alive
  (mkStdGen 70)                    -- Deterministic Random Number Generator
  [("astroid", 2.0),
   ("alien",   0.7)]               -- Spawn rates (in percentage)
  1                                -- Difficulty

imgWallpaper = "wallpaper.bmp" -- Source: self made in paint
imgPlayer    = "spaceship.bmp" -- Source: https://pixabay.com/nl/vectors/tekenfilm-ufo-ruimteschip-5181269/
imgAstroid   = "astroid.bmp"   -- Source: https://freepngimg.com/png/33922-asteroid
imgAlien     = "alien.bmp"     -- Source: https://clipartcraft.com/explore/spaceship-clipart-pixel/
imgBullet    = "bullet.bmp"    -- Source: self made in paint

main :: IO ()
main = do
  wallpaperImg <- loadBMP imgWallpaper
  playerImg    <- loadBMP imgPlayer
  astroidImg   <- loadBMP imgAstroid
  alienImg     <- loadBMP imgAlien
  bulletImg    <- loadBMP imgBullet
  playIO (InWindow "Shoot'm up" screenSize (0, 0)) -- Or FullScreen
        white              -- Background color
        60                 -- Frames per second
        initialState       -- Initial state
        (return . view screenSize
          [("wallpaper", wallpaperImg),
           ("player",    playerImg),
           ("astroid",   astroidImg),
           ("alien",     alienImg),
           ("bullet",    bulletImg)])  -- View function
        input   -- Event function
        ((return .) . step)               -- Step function