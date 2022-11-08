{-#LANGUAGE DuplicateRecordFields#-}

module Main where

import Controller
import Model
import View

import Graphics.Gloss.Interface.Pure.Game
import Graphics.Gloss
import System.Random

screenSize = (1000, 600)
initialState = GameState
  (Player (Coords (-350) 0) (60, 35) 500)   -- player
  []                               -- keyList
  [Alien   { pos = Coords 300 100,    rotation = 0, scaleEnemy = 1, size = (55, 15), speed = fst $ uniformR (10, 60) (mkStdGen 10), health = 1 },
   Astroid { pos = Coords 300 (-100), rotation = 0, scaleEnemy = 1, size = (30, 30), speed = fst $ uniformR (10, 30) (mkStdGen 70) }]        -- aliens
  []                           -- despawning enemies
  0                            -- elapsed time
  False                        -- paused
  True                         -- alive
  (mkStdGen 70)                -- Random Number Generator

imgWallpaper = "wallpaper.bmp" -- Source: self made in paint
imgPlayer    = "spaceship.bmp" -- Source: https://pixabay.com/nl/vectors/tekenfilm-ufo-ruimteschip-5181269/
imgAlien     = "alien.bmp"     -- Source: https://clipartcraft.com/explore/spaceship-clipart-pixel/
imgAstroid   = "astroid.bmp"   -- Source: https://freepngimg.com/png/33922-asteroid
imgBullet    = "bullet.bmp"    -- Source: self made in paint

main :: IO ()
main = do
  wallpaperImg <- loadBMP imgWallpaper
  playerImg    <- loadBMP imgPlayer
  astroidImg   <- loadBMP imgAstroid
  alienImg     <- loadBMP imgAlien
  bulletImg    <- loadBMP imgBullet
  -- all IO is loaded, now run the game in a pure environment
  play (InWindow "Shoot'm up" screenSize (0, 0)) -- Or FullScreen
        white              -- Background color
        60                 -- Frames per second
        initialState       -- Initial state
        (view screenSize [("wallpaper", wallpaperImg),
                          ("player",    playerImg),
                          ("astroid",   astroidImg),
                          ("alien",     alienImg),  -- View function
                          ("bullet",    bulletImg)])  -- View function
        input              -- Event function
        step               -- Step function