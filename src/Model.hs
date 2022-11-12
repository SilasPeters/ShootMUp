{-#LANGUAGE DuplicateRecordFields#-}
{-#LANGUAGE ExistentialQuantification#-}

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

data Coords = Coords { x :: CoordX, y :: CoordY }
instance Num Coords where
  (+) (Coords x y) (Coords p q) = Coords (x + p) (y + q)
  (-) (Coords x y) (Coords p q) = Coords (x - p) (y - q)
instance Eq Coords where
  (Coords x y) == (Coords p q) = x == p && y == q

times :: Coords -> Float -> Coords
(Coords x y) `times` n = Coords (x * n) (y * n)

-- New data types
--data CollidableType = forall a . Collidable a => CollidableType { entity :: a } -- aims to wrap players and enemies into a single list
data GameState = GameState { player :: Player, keyList :: [Char], enemies :: [Enemy], despawningEnemies :: [Enemy], t :: Time, paused :: Paused, alive :: Alive, rng :: StdGen, enemySpawnRates :: [(String, Float)], difficulty :: Float }
data Player    = Player    { pos :: Coords, size :: Size, pace :: Speed }
data Enemy     = Astroid   { pos :: Coords, rotation :: Rotation, scaleEnemy :: Scale, size :: Size, speed :: Speed }
               | Alien     { pos :: Coords, rotation :: Rotation, scaleEnemy :: Scale, size :: Size, speed :: Speed, health :: Health }
               | Bullet    { pos :: Coords, rotation :: Rotation, scaleEnemy :: Scale, size :: Size, direction :: Direction }

-- Classes
class Entity e where
  move         :: e -> Time -> CoordX -> CoordY -> e
  rotate       :: e -> Rotation -> e
  getPos       :: e -> Coords
  getScale     :: e -> Scale
  getSize      :: e -> Size
  getRotation  :: e -> Rotation
  entityId     :: e -> String
  --getSpawnRate :: e -> GameState -> SpawnRate

  --getSpawnRate e gs = fromJust $ lookup (entityId e) (enemySpawnRates gs)

class Entity e => Collidable e where
  collidesWith :: (Entity m) => e -> m -> Bool
  onCollide    :: e -> GameState -> GameState

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
  onCollide e gs = trace "playerCollision" gs { alive = False }

instance Collidable Enemy where
  onCollide e gs = trace "enemyCollision" gs { enemies = removeItem e (enemies gs), despawningEnemies = e : despawningEnemies gs }

instance ShootingEntity Player where
  shoot p@Player {pos = pos} gs =
    gs { enemies =
         Bullet {pos = pos { x = x pos + shipWidth + 10}, rotation = 0, scaleEnemy = 1, size = (3,3), direction = (100, 0) }
       : enemies gs}

instance ShootingEntity Enemy where
  shoot e@Alien {pos = Coords alienx alieny} gs@GameState { player = p, enemies = enemies } =
    let shipx = x (getPos p)
        shipy = y (getPos p)
    in gs { enemies =
        Bullet { pos = Coords alienx alieny, rotation = 0, scaleEnemy = 1, size = (3,3), direction = (alienx - shipx, alieny - shipy) }
       : enemies}