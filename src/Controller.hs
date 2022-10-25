-- | This module defines how the state changes
--   in response to time and user input
module Controller where

import Model

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import System.Random
import qualified Data.Set as S

pace = 5

-- | Handle one iteration of the game
step :: Time -> GameState -> IO GameState
step _ gs@(GameState _ keylist _ _ _) | 'u' `elem` keylist = return (movePlayer gs pace)
                                      | 'd' `elem` keylist = return (movePlayer gs (-pace))
                                      | otherwise = return gs

-- | Handle user input
input :: Event -> GameState -> IO GameState
input e gs = return (inputKey e gs)

inputKey :: Event -> GameState -> GameState
inputKey (EventKey (SpecialKey KeyUp) Down _ _) gs@(GameState _ keylist _ _ _) = updateKeyList gs ('u' : keylist)
inputKey (EventKey (SpecialKey KeyUp) Up _ _) gs@(GameState _ keylist _ _ _) = updateKeyList gs (removeItem 'u' keylist)
inputKey (EventKey (SpecialKey KeyDown) Down _ _) gs@(GameState _ keylist _ _ _) = updateKeyList gs ('d' : keylist)
inputKey (EventKey (SpecialKey KeyDown) Up _ _) gs@(GameState _ keylist _ _ _) = updateKeyList gs (removeItem 'd' keylist)
inputKey _ gs = gs

updateKeyList :: GameState -> [Char] -> GameState
updateKeyList (GameState player keylist enemies time paused) keys = GameState player keys enemies time paused

removeItem :: (Eq a) => a -> [a] -> [a]
removeItem _ []                 = []
removeItem x (y:ys) | x == y    = removeItem x ys
                    | otherwise = y : removeItem x ys
-- https://stackoverflow.com/questions/2097501/learning-haskell-how-to-remove-an-item-from-a-list-in-haskell

movePlayer :: GameState -> CoordY -> GameState
movePlayer (GameState player keylist aliens t paused) dy = GameState (move player 0 dy) keylist aliens t paused

