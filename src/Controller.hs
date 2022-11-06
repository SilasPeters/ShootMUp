-- | This module defines how the state changes
--   in response to time and user input
module Controller where

import Model
import Graphics.Gloss.Interface.Pure.Game hiding (rotate)
import SupportiveFunctions
import System.Random
import Data.List


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
checkCollisions gs = let firstCollisionOfEnemy  = find (`collidesWith` player gs) $ enemies gs
                         firstCollisionOfPlayer = find (player gs `collidesWith`) $ enemies gs
                      in case firstCollisionOfEnemy of -- only interprets first found collision, because handling one collision per frame is enough and otherwise gamestate has to be a state monad...
                        Just x -> onCollide x gs
                        _      -> case firstCollisionOfPlayer of
                           Just y -> onCollide y gs
                           _ -> gs
                     -- let collidables = player gs : enemies gs
                     --     firstCollision = find (`collidesWith` collidables) collidables -- only interprets first found collision, because handling one collision per frame is enough and otherwise gamestate has to be a state monad...
                     --  in onCollide firstCollision
                     -- --     collisions = mapMaybe (`collidesWith` collidables) collidables
                     -- --  in map (onCollide gs) collisions <<<--- needs to work with states

applyEnemyLogic :: GameState -> Time -> (Enemy, Int) -> GameState
applyEnemyLogic gs dt (e@Astroid {}, i) = updateEnemyAt i gs $ rotate (move e dt (-speed e) 0) (8 * dt)
applyEnemyLogic gs dt (e@Alien {},   i) = updateEnemyAt i gs $ move e dt (-speed e) $ fst $ uniformR (-1, 1) (rng gs)
applyEnemyLogic gs dt (e@Bullet {},  i) = updateEnemyAt i gs $ uncurry (move e dt) (direction e)

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