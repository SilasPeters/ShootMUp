module Main where

import Controller
import Model
import View

import Graphics.Gloss.Interface.IO.Game
screenSize = (1000, 600)
initialState = GameState (Player 0) [Alien (Coords (300, 100)) 1] 0 False


main :: IO ()
main = playIO (InWindow "Shoot'm up" screenSize (0, 0)) -- Or FullScreen
              white              -- Background color
              60                 -- Frames per second
              initialState       -- Initial state
              (view screenSize)  -- View function
              input              -- Event function
              step               -- Step function