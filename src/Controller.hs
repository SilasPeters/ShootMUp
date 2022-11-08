-- | This module defines how the state changes
--   in response to time and user input
module Controller where

import Model
import Graphics.Gloss.Interface.Pure.Game hiding (rotate)
import SupportiveFunctions
import System.Random
import Data.List
import Data.Maybe

-- | Handle one iteration of the game
step :: Time -> GameState -> GameState
step dt gs = if skipFrame then gs else (despawnEnemies . checkCollisions . enemiesLogic dt . playerLogic . processInput dt . incrementTime dt) gs
   where skipFrame = paused gs || not (alive gs) -- do not perform logic if the game is paused or the game is over

incrementTime :: Time -> GameState -> GameState
incrementTime dt gs = gs { t = t gs + dt }

processInput :: Time -> GameState -> GameState -- keyboard wordt hier verwerkt
processInput dt gs@GameState { keyList = kl, player = p } = foldl f gs kl
   where
      f :: GameState -> Char -> GameState
      f gs 'u' = gs { player = move p dt 0 ( pace p) }
      f gs 'd' = gs { player = move p dt 0 (-pace p) }
      f gs 'r' = shoot p gs
      f gs _   = gs
         -- spacebar key is handled by inputKey, so that a paused game can be unpaused

playerLogic :: GameState -> GameState
playerLogic = id

enemiesLogic :: Time -> GameState -> GameState
enemiesLogic dt gs = foldl (`applyEnemyLogic` dt) gs $ zip (enemies gs) [0..] -- zipping gives applyEnemyLogic both the enemy and its index in the list

checkCollisions :: GameState -> GameState
checkCollisions gs = let collision :: (Collidable a, Collidable b) => a -> b -> Maybe a
                         collision p q = if p `collidesWith` q then Just p else Nothing
                         collisionsPlayerEnemies  = mapMaybe (player gs `collision`) (enemies gs)
                         collisionsEnemiesPlayer  = mapMaybe (`collision` player gs) (enemies gs)
                         collisionsEnemiesEnemies = concatMap (\c -> mapMaybe (c `collision`) (enemies gs)) (enemies gs)
                      in foldl f (foldl f gs (collisionsEnemiesPlayer ++ collisionsEnemiesEnemies)) collisionsPlayerEnemies
                      where
                        f :: Collidable e => GameState -> e -> GameState
                        f gs e = onCollide e gs

despawnEnemies :: GameState -> GameState
despawnEnemies gs = gs {despawningEnemies = scalingEnemies gs (despawningEnemies gs)}

scalingEnemies :: GameState -> [Enemy] -> [Enemy]
scalingEnemies gs [] = []
scalingEnemies gs (e:es) = if (scaleEnemy e) < 0.01 then removeItem e (despawningEnemies gs) else e { scaleEnemy = scaleEnemy e * 0.5} : scalingEnemies gs es

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
input (EventKey (SpecialKey KeySpace) Down _ _) gs = gs { paused = not (paused gs) }
input _ gs = gs