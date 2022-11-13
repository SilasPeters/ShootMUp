{-#LANGUAGE DuplicateRecordFields#-}
{-#LANGUAGE ExistentialQuantification#-}
{-# LANGUAGE DeriveGeneric #-}

-- | This module contains the data types
--   which represent the state of the game
module Model where
import Prelude hiding (lookup)
import Graphics.Gloss (Picture, Vector)
import SupportiveFunctions
import System.Random
import Data.List
import Debug.Trace
import Data.Maybe
import GHC.Generics
import Data.Aeson as JSON

shipMaxY = 265
shipWidth = 60
shipHeigth = 35

-- Our primitives
type Paused     = Bool
type Alive      = Bool
type Time       = Float
type CoordX     = Float
type CoordY     = Float
type Health     = Float
type Size       = (Float, Float)
type Scale      = Float
type Rotation   = Float
type Speed      = Float
type Direction  = Vector
type SpawnRate  = Float -- in percentage
type EntityId   = String
type Difficulty = Float

data Coords = Coords { x :: CoordX, y :: CoordY } deriving (Generic, Show)
instance Num Coords where
  (+) (Coords x y) (Coords p q) = Coords (x + p) (y + q)
  (-) (Coords x y) (Coords p q) = Coords (x - p) (y - q)
instance Eq Coords where
  (Coords x y) == (Coords p q) = x == p && y == q

times :: Coords -> Float -> Coords
(Coords x y) `times` n = Coords (x * n) (y * n)

-- New data types
data GameState = GameState { player :: Player, keyList :: [Char], enemies :: [Enemy], despawningEnemies :: [Enemy], t :: Time, paused :: Paused, alive :: Alive, rng :: StdGen, enemySpawnRates :: [(String, Float)], difficulty :: Float }
data Player    = Player    { pos :: Coords, size :: Size, pace :: Speed }
                  deriving (Generic, Show)
data Enemy     = Astroid   { pos :: Coords, rotation :: Rotation, scaleEnemy :: Scale, size :: Size, speed :: Speed }
               | Alien     { pos :: Coords, rotation :: Rotation, scaleEnemy :: Scale, size :: Size, speed :: Speed, health :: Health }
               | Bullet    { pos :: Coords, rotation :: Rotation, scaleEnemy :: Scale, size :: Size, direction :: Direction }
                  deriving (Generic, Show)

-- JSON Serialization
data GameStateSerializable = GameStateSerializable Player [Char] [Enemy] [Enemy] Time Paused Alive [(String, Float)] Float
  deriving (Generic, Show)
  -- Its meets the exact same pattern as GameState, but excludes rng :: StdGen
instance ToJSON Coords where
instance ToJSON Enemy where
instance ToJSON Player where
instance ToJSON GameStateSerializable where
instance FromJSON Enemy
instance FromJSON Coords
instance FromJSON Player
instance FromJSON GameStateSerializable

-- Classes
class Entity e where
  move         :: e -> Time -> CoordX -> CoordY -> e
  rotate       :: e -> Rotation -> e
  getPos       :: e -> Coords
  getScale     :: e -> Scale
  getSize      :: e -> Size
  getRotation  :: e -> Rotation
  entityId     :: e -> String

class Entity e => Collidable e where
  collidesWith :: (Entity m) => e -> m -> Bool
  onCollide    :: e -> GameState -> IO GameState

  collidesWith e o = let (Coords ex ey) = getPos e
                         (Coords ox oy) = getPos o
                         (esizex, esizey) = getSize e
                         (osizex, osizey) = getSize o
                      in (ex - esizex < ox + osizex && ox - osizex < ex + esizex)
                      && (ey - esizey < oy + osizey && oy + osizey < ey + esizey
                       || ey + esizey > oy - osizey && oy - osizey > ey - esizey)

class ShootingEntity e where
  shoot :: e -> GameState -> GameState

-- Instances and commonalities
instance Entity Player where
  move p@Player { pos = pos } dt _ dy = p { pos = pos { y = newClampedY } }
    where newClampedY = clamp (-shipMaxY, shipMaxY) (y pos + (dy * dt))
  getPos = pos
  getSize = size
  entityId = const "player"

instance Entity Enemy where
  move e@Astroid { pos = pos } dt dx dy = e { pos = pos + Coords (dx * dt) (dy * dt)}
  move e@Alien   { pos = pos } dt dx dy = e { pos = pos + Coords (dx * dt) (dy * dt)}
  move e@Bullet  { pos = pos } dt dx dy = e { pos = pos + Coords (dx * dt) (dy * dt)}
  rotate e@Astroid { rotation = rotation } degree = e { rotation = rotation + degree}
  rotate e@Alien   { rotation = rotation } degree = e { rotation = rotation + degree}
  rotate e@Bullet  { rotation = rotation } degree = e { rotation = rotation + degree}
  getPos = pos
  getSize = size
  getScale = scaleEnemy
  getRotation = rotation
  entityId Alien   {} = "alien"
  entityId Astroid {} = "astroid"
  entityId Bullet  {} = "bullet"

instance Eq Enemy where
  a == b = getPos a == getPos b

instance Collidable Player where
  onCollide e gs = do
    savePlayedTime $ t gs
    return gs { alive = False }

timeLocation = "time.txt"

savePlayedTime :: Time -> IO ()
savePlayedTime = writeFile timeLocation . show

instance Collidable Enemy where
  onCollide e gs = return gs { enemies = removeItem e (enemies gs), despawningEnemies = e : despawningEnemies gs }

instance ShootingEntity Player where
  shoot p@Player {pos = pos} gs =
    gs { enemies = enemies gs ++
         [Bullet {pos = pos { x = x pos + shipWidth + 10}, rotation = 0, scaleEnemy = 1, size = (3,3), direction = (100, 0) }]
       }

instance ShootingEntity Enemy where
  shoot e@Alien {} gs@GameState { player = p, enemies = enemies } =
    let shipx = x (getPos p)
        shipy = y (getPos p)
        bulletSpeedMagnitude = 0.3
        bulletDirection = ((shipx - x (getPos e)) * bulletSpeedMagnitude, (shipy - y (getPos e)) * bulletSpeedMagnitude)
    in gs { enemies = enemies ++
        [Bullet { pos = getPos e - Coords (20 + fst (getSize e)) 0, rotation = 0, scaleEnemy = 0.7, size = (3,3), direction = bulletDirection }]
       }
  shoot Astroid {} gs = gs