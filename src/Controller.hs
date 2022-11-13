-- | This module defines how the state changes
--   in response to time and user input
module Controller where

import Graphics.Gloss.Interface.Pure.Game hiding (rotate)
import SupportiveFunctions
import System.Random
import Data.List
import Debug.Trace
import Data.Maybe
import Model
import Data.Aeson as JSON
--import Data.ByteString.Lazy.Internal
--import Data.ByteString.Lazy.UTF8 as ByteStuff (ByteString)
import Data.ByteString as ByteStuff (readFile)

-- | Handle one iteration of the game
step :: Time -> GameState -> GameState
step dt gs =
     if paused gs || not (alive gs) -- do not perform logic if the game is paused or the game is over
   then gs
   else (despawnEnemies . checkCollisions . enemiesLogic dt . spawnEnemies . playerLogic . processInput dt . incrementDifficulty . incrementTime dt) gs


incrementTime :: Time -> GameState -> GameState
incrementTime dt gs = gs { t = t gs + dt }

incrementDifficulty :: GameState -> GameState
incrementDifficulty gs = gs { difficulty = difficulty gs * 1.0001 }

processInput :: Time -> GameState -> GameState -- keyboard wordt hier verwerkt
processInput dt gs@GameState { player = p } = foldl f gs (keyList gs)
   where
      f :: GameState -> Char -> GameState
      f gs 'u' = gs { player = move p dt 0 ( pace p) }
      f gs 'd' = gs { player = move p dt 0 (-pace p) }
      f gs 'r' = shoot p gs
      f gs _   = gs
         -- spacebar key is handled by inputKey, so that a paused game can be unpaused

playerLogic :: GameState -> GameState
playerLogic = id

spawnEnemies :: GameState -> GameState
spawnEnemies gs@GameState { rng = rng } = spawnEnemy (spawnEnemy gs "astroid") "alien"

spawnEnemy :: GameState -> EntityId -> GameState
spawnEnemy gs entityId =
   let (spawnRoll,  newRandom  ) = uniformR (0,    100) (rng gs)
       (enemyYRoll, newRandom' ) = uniformR (-300, 300) newRandom
       (enemySpeed, newRandom'') = uniformR (50,   300) newRandom'
    in if spawnRoll / difficulty gs <= getSpawnRate gs entityId
      then gs { rng = newRandom'', enemies = createEnemy enemyYRoll enemySpeed : enemies gs }
      else gs { rng = newRandom }
   where
      createEnemy :: Float -> Speed -> Enemy
      createEnemy y speed = case entityId of
         "astroid" -> Astroid (Coords 550 y) 0 1 (40, 40) speed
         "alien"   -> Alien   (Coords 550 y) 0 1 (40, 40) speed 2
         _         -> error $ "Can't spawn enemy of kind " ++ entityId

getSpawnRate :: GameState -> EntityId -> SpawnRate
getSpawnRate gs id = fromJust $ lookup id (enemySpawnRates gs)

enemiesLogic :: Time -> GameState -> GameState
enemiesLogic dt gameState = let relevantEnemies = despawnOutOfBounds (enemies gameState)
                             in applyEnemyLogics dt (gameState { enemies = relevantEnemies })
   where
      despawnOutOfBounds enemies = mapMaybe (\e -> if outOfBounds e then Nothing else Just e) enemies
      outOfBounds e = let ex = x (getPos e) in ex < -500 || ex > 550

      applyEnemyLogics dt gs' = foldl (`applyEnemyLogic` dt) gs' $ zip (enemies gs') [0..] -- zipping gives applyEnemyLogic both the enemy and its index in the list

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
scalingEnemies gs [] = [];
scalingEnemies gs (e:es) =
   if   scaleEnemy e < 0.01
   then removeItem e (despawningEnemies gs)
   else e { scaleEnemy = scaleEnemy e * 0.3} : scalingEnemies gs es

applyEnemyLogic :: GameState -> Time -> (Enemy, Int) -> GameState
applyEnemyLogic gs dt (e@Astroid {}, i) = updateEnemyAt i gs $ rotate (move e dt (-speed e) 0) (8 * dt)
applyEnemyLogic gs dt (e@Alien {},   i) = let (randomY,   newRng ) = uniformR (-50, 50) (rng gs)
                                              (shootRoll, newRng') = uniformR (0, 100) newRng
                                              newGameState = gs {rng = newRng'}
                                              movedEnemy = move e dt (-speed e) randomY
                                              gsAfterShoot = if shootRoll <= (2 :: Float)
                                                           then shoot movedEnemy newGameState
                                                           else newGameState
                                           in updateEnemyAt i gsAfterShoot movedEnemy
applyEnemyLogic gs dt (e@Bullet {},  i) = updateEnemyAt i gs $ uncurry (move e dt) (direction e)

updateEnemyAt :: Int -> GameState -> Enemy -> GameState -- replaces the enemy at the given index in the list of enemies in the gs, with a new value
updateEnemyAt i gs enemy = gs { enemies = replaceAt i enemy (enemies gs) }

input :: Event -> GameState -> IO GameState
input (EventKey (SpecialKey KeyUp)    Down _ _) gs = return gs { keyList = 'u' : keyList gs}
input (EventKey (SpecialKey KeyUp)    Up   _ _) gs = return gs { keyList = removeItem 'u' (keyList gs)}
input (EventKey (SpecialKey KeyDown)  Down _ _) gs = return gs { keyList = 'd' : keyList gs}
input (EventKey (SpecialKey KeyDown)  Up   _ _) gs = return gs { keyList = removeItem 'd' (keyList gs)}
input (EventKey (SpecialKey KeyRight) Down _ _) gs = return gs { keyList = 'r' : keyList gs}
input (EventKey (SpecialKey KeyRight) Up   _ _) gs = return gs { keyList = removeItem 'r' (keyList gs)}
input (EventKey (SpecialKey KeySpace) Down _ _) gs = return gs { paused = not (paused gs) }
input (EventKey (Char 'i')            Down _ _) gs = loadStateFromJSON
input (EventKey (Char 'o')            Down _ _) gs = saveStateToJSON gs >> return gs
input _ gs = return gs


-- After this follows all stuff to store and load the gamestate in and from JSON
stateJSONLocation = "savedGameState.json"

saveStateToJSON :: GameState -> IO ()
saveStateToJSON = writeFile stateJSONLocation . show . JSON.encode . serializeable

loadStateFromJSON :: IO GameState
loadStateFromJSON = do
   stateString <- ByteStuff.readFile stateJSONLocation
   case JSON.decode stateString of
      Just x  -> return $ deserializeable x
      Nothing -> error "Could not load save state. Please create one first by pressing \"O\" (as in Output)"

serializeable :: GameState -> GameStateSerializable
serializeable (GameState p kl es des t paused alive _ sr difficulty)
   = GameStateSerializable p kl es des t paused alive sr difficulty

deserializeable :: GameStateSerializable -> GameState
deserializeable (GameStateSerializable p kl es des t paused alive sr difficulty)
   = GameState p kl es des t paused alive rng sr difficulty
   where rng = mkStdGen 70