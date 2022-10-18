-- | This module contains the data types
--   which represent the state of the game
module Model where
import Distribution.Simple (registrationPackageDB)
import Graphics.Gloss

initialState :: GameState
initialState = GameState (Player (CoordY 0)) [] (Time 0) (Paused False)

-- Gamestate
newtype Paused = Paused Bool
newtype Time   = Time Int

-- General stuff
data GameState = GameState Player [Alien] Time Paused
newtype CoordX = CoordX Float
newtype CoordY = CoordY Float
newtype Health = Health Float
data Size      = Size Float Float

-- Entities
newtype Player = Player CoordY
data Astroid   = Astroid Size (CoordX, CoordY)
data Alien     = Alien (CoordX, CoordY) Health

instance Num CoordX where
  (+) (CoordX a) (CoordX b) = CoordX (a + b)
  (-) (CoordX a) (CoordX b) = CoordX (a - b)
  (*) (CoordX a) (CoordX b) = CoordX (a * b)
instance Num CoordY where
  (+) (CoordY a) (CoordY b) = CoordY (a + b)
  (-) (CoordY a) (CoordY b) = CoordY (a - b)
  (*) (CoordY a) (CoordY b) = CoordY (a * b)

-- Classes
class Entity e where
  move      :: e -> CoordX -> CoordY -> e
  display   :: e -> Picture  

class Entity e => CollidableEntity e where
  collides  :: e -> e -> bool -- of zo
  onCollide :: e -> e

class Entity e => ShootingEntity e where
  shoot :: e -> IO io  

instance Entity Player where
  move (Player y) _ dy = Player (y + dy) -- afhankelijk van keyboard
instance ShootingEntity Player where
  shoot (Player y) = undefined

instance Entity Astroid where
   move (Astroid sz (x, y)) dx dy = Astroid sz (x + dx, y + dy)