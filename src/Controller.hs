-- | This module defines how the state changes
--   in response to time and user input
module Controller where

import Model
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import Control.Monad.Trans.State
--import System.Random

pace = 5

-- | Handle one iteration of the game
-- step t gs@(GameState player keylist enemies time paused) | 'u' `elem` keylist = return (updateGameState gs t pace)
--                                       | 'd' `elem` keylist = return (updateGameState gs t (-pace))
--                                       | ' ' `elem` keylist = return (GameState player keylist enemies time not(paused))
--                                       | otherwise = return (updateGameState gs t 0)

-- step t gs = return (execState (do
--   dingEen
--   dingTwee
--   get) gs)

step :: Time -> GameState -> IO GameState
step dt = return . enemiesLogic . processInput . incrementTime

incrementTime :: GameState -> GameState
incrementTime = id
processInput :: GameState -> GameState -- keyboard wordt hier verwerkt
processInput = id
enemiesLogic :: GameState -> GameState
enemiesLogic = id

-- | Handle user input
input :: Event -> GameState -> IO GameState
input e gs = return (inputKey e gs)

inputKey :: Event -> GameState -> GameState
inputKey (EventKey (SpecialKey KeyUp) Down _ _) gs@(GameState _ keylist _ _ _) = updateKeyList gs ('u' : keylist)
inputKey (EventKey (SpecialKey KeyUp) Up _ _) gs@(GameState _ keylist _ _ _) = updateKeyList gs (removeItem 'u' keylist)
inputKey (EventKey (SpecialKey KeyDown) Down _ _) gs@(GameState _ keylist _ _ _) = updateKeyList gs ('d' : keylist)
inputKey (EventKey (SpecialKey KeyDown) Up _ _) gs@(GameState _ keylist _ _ _) = updateKeyList gs (removeItem 'd' keylist)
inputKey (EventKey (SpecialKey KeyRight) Down _ _) gs@(GameState _ keylist _ _ _) = updateKeyList gs ('r' : keylist)
inputKey (EventKey (SpecialKey KeyRight) Up _ _) gs@(GameState _ keylist _ _ _) = updateKeyList gs (removeItem 'r' keylist)
inputKey (EventKey (SpecialKey KeySpace) Down _ _) gs@(GameState _ keylist _ _ _) = updateKeyList gs (' ' : keylist)
inputKey (EventKey (SpecialKey KeySpace) Up _ _) gs@(GameState _ keylist _ _ _) = updateKeyList gs (removeItem ' ' keylist)
inputKey _ gs = gs

updateKeyList :: GameState -> [Char] -> GameState
updateKeyList (GameState player keylist enemies time paused) keys = GameState player keys enemies time paused

updateGameState :: GameState -> Time -> CoordY -> GameState
updateGameState (GameState player keylist enemies time paused) t dy = (GameState (move player 0 dy) keylist enemies (time+t) paused)

removeItem :: (Eq a) => a -> [a] -> [a]
removeItem _ []                 = []
removeItem x (y:ys) | x == y    = removeItem x ys
                    | otherwise = y : removeItem x ys
-- Source: https://stackoverflow.com/questions/2097501/learning-haskell-how-to-remove-an-item-from-a-list-in-haskell

movePlayer :: GameState -> CoordY -> GameState
movePlayer (GameState player keylist aliens t paused) dy = GameState (move player 0 dy) keylist aliens t paused

