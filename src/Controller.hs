-- | This module defines how the state changes
--   in response to time and user input
module Controller where

import Model

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import System.Random

-- | Handle one iteration of the game
step :: Float -> GameState -> IO GameState
step _ = return

-- | Handle user input
input :: Event -> GameState -> IO GameState
input e gs = return (inputKey e gs)

inputKey :: Event -> GameState -> GameState
inputKey (EventKey (SpecialKey KeyUp) Down _ _) (GameState (Player (CoordY y)))  = GameState (Player (CoordY (y+10)))
inputKey (EventKey (SpecialKey KeyDown) Down _ _) (GameState (Player (CoordY y) ))  = GameState (Player (CoordY (y-10)))
inputKey _ gs = gs