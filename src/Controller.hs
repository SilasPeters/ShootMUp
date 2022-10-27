-- | This module defines how the state changes
--   in response to time and user input
module Controller where

import Model
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
--import System.Random

pace = 5

-- | Handle one iteration of the game
step :: Time -> GameState -> IO GameState
step dt gs = (return . enemiesLogic . processInput . incrementTime dt) gs

incrementTime :: Time -> GameState -> GameState
incrementTime t gs  = gs { time = (floor((time+t) * 10^2)/10^2) }

processInput :: GameState -> GameState -- keyboard wordt hier verwerkt
processInput gs 
   | 'u' `elem` keylist = return gs { player = (move player 0 pace) }
   | 'd' `elem` keylist = return gs { player = (move player 0 (-pace)) }
--   | 'r' `elem` keylist = return (GameState (shoot player) keylist enemies time paused)
--   | otherwise = return (GameState player keylist enemies time paused) 

-- | Handle user input
input :: Event -> GameState -> GameState
input e gs = inputKey e gs

inputKey :: Event -> GameState -> GameState
inputKey (EventKey (SpecialKey KeyUp) Down _ _) gs@(GameState _ keylist _ _ _) = updateKeyList gs ('u' : keylist)
inputKey (EventKey (SpecialKey KeyUp) Up _ _) gs@(GameState _ keylist _ _ _) = updateKeyList gs (removeItem 'u' keylist)
inputKey (EventKey (SpecialKey KeyDown) Down _ _) gs@(GameState _ keylist _ _ _) = updateKeyList gs ('d' : keylist)
inputKey (EventKey (SpecialKey KeyDown) Up _ _) gs@(GameState _ keylist _ _ _) = updateKeyList gs (removeItem 'd' keylist)
inputKey (EventKey (SpecialKey KeyRight) Down _ _) gs@(GameState _ keylist _ _ _) = updateKeyList gs ('r' : keylist)
inputKey (EventKey (SpecialKey KeyRight) Up _ _) gs@(GameState _ keylist _ _ _) = updateKeyList gs (removeItem 'r' keylist)
inputKey (EventKey (SpecialKey KeySpace) Down _ _) gs = gs { paused = (not(paused)) }
inputKey _ gs = gs

updateKeyList :: GameState -> [Char] -> GameState
updateKeyList gs keys = gs { keylist = keys } 

updateGameState :: GameState -> CoordY -> GameState
updateGameState (GameState player keylist enemies time paused) dy = (GameState (move player 0 dy) keylist enemies time paused)

removeItem :: (Eq a) => a -> [a] -> [a]
removeItem _ []                 = []
removeItem x (y:ys) | x == y    = removeItem x ys
                    | otherwise = y : removeItem x ys
-- Source: https://stackoverflow.com/questions/2097501/learning-haskell-how-to-remove-an-item-from-a-list-in-haskell

movePlayer :: GameState -> CoordY -> GameState
movePlayer gs  dy = gs { player = (move player 0 dy) }