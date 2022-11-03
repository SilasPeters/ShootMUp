-- | This module defines how the state changes
--   in response to time and user input
module Controller where

import Model
import Graphics.Gloss.Interface.Pure.Game hiding (rotate)
import SupportiveFunctions
import System.Random


-- | Handle one iteration of the game
step :: Time -> GameState -> GameState
step dt gs = if skipFrame then gs else (checkCollisions . enemiesLogic dt . playerLogic . processInput dt . incrementTime dt) gs
   where skipFrame = paused gs || not (alive gs) -- do not perform logic if the game is paused or the game is over

incrementTime :: Time -> GameState -> GameState
incrementTime dt gs = gs { t = t gs + dt }

processInput :: Time -> GameState -> GameState -- keyboard wordt hier verwerkt
processInput dt gs@GameState { keyList = kl, player = p }
   | 'u' `elem` kl = gs { player = move p dt 0 (pace p) }
   | 'd' `elem` kl = gs { player = move p dt 0 (-pace p) }
   | 'r' `elem` kl = shoot p gs
   -- spacebar key is handled by inputKey TODO: make this more obvious
   | otherwise = gs

playerLogic :: GameState -> GameState
playerLogic = id

enemiesLogic :: Time -> GameState -> GameState
enemiesLogic dt gs = foldl (`applyEnemyLogic` dt) gs $ zip (enemies gs) [0..] -- zipping gives applyEnemyLogic both the enemy and its index in the list

checkCollisions :: GameState -> GameState
checkCollisions gs = if any (collidesWithPlayer (player gs)) (enemies gs) then gs {alive = False} else gs

applyEnemyLogic :: GameState -> Time -> (Enemy, Int) -> GameState
applyEnemyLogic gs dt (e@Astroid {}, i) = updateEnemyAt i gs $ rotate (move e dt (-speed e) 0) (8 * dt)
applyEnemyLogic gs dt (e@Alien {},   i) = updateEnemyAt i gs $ move e dt (-speed e) $ fst $ uniformR (-1, 1) (rng gs)

updateEnemyAt :: Int -> GameState -> Enemy -> GameState -- replaces the enemy at the given index in the list of enemies in the gs, with a new value
updateEnemyAt i gs enemy = gs { enemies = replaceAt i enemy (enemies gs) }

input :: Event -> GameState -> GameState
input (EventKey (SpecialKey KeyUp)    Down _ _) gs = gs { keyList = 'u' : keyList gs}
input (EventKey (SpecialKey KeyUp)    Up   _ _) gs = gs { keyList = removeItem 'u' (keyList gs)}
input (EventKey (SpecialKey KeyDown)  Down _ _) gs = gs { keyList = 'd' : keyList gs}
input (EventKey (SpecialKey KeyDown)  Up   _ _) gs = gs { keyList = removeItem 'd' (keyList gs)}
input (EventKey (SpecialKey KeyRight) Down _ _) gs = gs { keyList = 'r' : keyList gs}
input (EventKey (SpecialKey KeyRight) Up   _ _) gs = gs { keyList = removeItem 'r' (keyList gs)}
input (EventKey (SpecialKey KeySpace) Down _ _) gs = gs { paused  = not (paused gs) }
input _ gs = gs

removeItem :: (Eq a) => a -> [a] -> [a]
removeItem _ []                 = []
removeItem x (y:ys) | x == y    = removeItem x ys
                    | otherwise = y : removeItem x ys -- todo: kan korter
-- Source: https://stackoverflow.com/questions/2097501/learning-haskell-how-to-remove-an-item-from-a-list-in-haskell