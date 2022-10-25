-- | This module contains the data types
--   which represent the state of the game
module Model where
--import Distribution.Simple (registrationPackageDB)
import Graphics.Gloss (Picture)

shipMaxY = 220
imgPlayer = "spaceship.bmp"
imgAlien = "alien.bmp" -- Source: https://clipartcraft.com/explore/spaceship-clipart-pixel/

-- Our primitives
type Paused    = Bool
type Time      = Float
type CoordX    = Float
type CoordY    = Float
newtype Coords = Coords (CoordX, CoordY)
type Health    = Float

instance Num Coords where
  (+) (Coords (x, y)) (Coords (p, q)) = Coords (x + y, p + q)
  (-) (Coords (x, y)) (Coords (p, q)) = Coords (x - y, p - q)

-- General stuff
data GameState = GameState Player [Char] [Alien] Time Paused
data Size      = Size Float Float

-- Entities
newtype Player = Player CoordY
data Astroid   = Astroid Coords Size
data Alien     = Alien Coords Health

-- Classes
class Entity e where
  move      :: e -> CoordX -> CoordY -> e
  display   :: e -> (Coords -> FilePath -> IO Picture) -> IO Picture

class Entity e => CollidableEntity e where
  collides  :: e -> e -> bool -- of zo
  onCollide :: e -> e

class Entity e => ShootingEntity e where
  shoot :: e -> IO io

-- Instances
instance Entity Player where
  move (Player y) _ dy = Player $ clamp (-shipMaxY, shipMaxY) (y + dy)
  display (Player y) f = f (Coords (-350, y)) imgPlayer
instance Entity Alien where
  display (Alien coords _) f = f coords imgAlien

instance ShootingEntity Player where
  shoot (Player y) = undefined

instance Entity Astroid where
   move (Astroid coords size) dx dy = Astroid (coords + Coords(dx, dy)) size


-- Supportive methods

clamp :: Ord a => (a, a) -> a -> a
clamp (min, max) x | x < min   = min
                   | x > max   = max
                   | otherwise = x