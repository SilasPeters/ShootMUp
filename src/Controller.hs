-- | This module defines how the state changes
--   in response to time and user input
module Controller where

import Model
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
--import System.Random

pace = 5

-- | Handle one iteration of the game
step :: Time -> GameState -> GameState
step dt = enemiesLogic . incrementTime dt . processInput

incrementTime :: Time -> GameState -> GameState
incrementTime dt gs = gs { t = t gs + dt }

processInput :: GameState -> GameState -- keyboard wordt hier verwerkt
processInput gs@GameState { keyList = kl, player = p}
   | 'u' `elem` kl = gs { player = move p 0 pace }
   | 'd' `elem` kl = gs { player = move p 0 (-pace) }
   | 'r' `elem` kl = gs { player = shoot p }
   | otherwise = gs

enemiesLogic :: GameState -> GameState
enemiesLogic = id

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
inputKey (EventKey (SpecialKey KeySpace) Down _ _) gs = gs { paused = not (paused gs) }
inputKey _ gs = gs

updateKeyList :: GameState -> [Char] -> GameState
updateKeyList gs keys = gs { keyList = keys }

--updateGameState :: GameState -> CoordY -> GameState
--updateGameState (GameState player keylist enemies time paused) dy = GameState (move player 0 dy) keylist enemies time paused

removeItem :: (Eq a) => a -> [a] -> [a]
removeItem _ []                 = []
removeItem x (y:ys) | x == y    = removeItem x ys
                    | otherwise = y : removeItem x ys -- todo: kan korter
-- Source: https://stackoverflow.com/questions/2097501/learning-haskell-how-to-remove-an-item-from-a-list-in-haskell

movePlayer :: GameState -> CoordY -> GameState
movePlayer gs dy = gs { player = move (player gs) 0 dy }