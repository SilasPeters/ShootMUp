{-#LANGUAGE DuplicateRecordFields#-}

-- | This module contains the data types
--   which represent the state of the game
module Model where
--import Distribution.Simple (registrationPackageDB)
import Graphics.Gloss (Picture)
import SupportiveFunctions

shipMaxY = 220

-- Our primitives
type Paused = Bool
type Time   = Float
type CoordX = Float
type CoordY = Float
type Health = Float
type Size   = Float

data Coords = Coords { x :: CoordX, y :: CoordY }
instance Num Coords where
  (+) (Coords x y) (Coords p q) = Coords (x + y) (p + q)
  (-) (Coords x y) (Coords p q) = Coords (x - y) (p - q)

-- New data types
data GameState = GameState { player :: Player, keyList :: [Char], enemies :: [Enemy], t :: Time, paused :: Paused }
newtype Player = Player    { pos :: Coords }
data Enemy     = Astroid   { pos :: Coords, size :: Size } | Alien { pos :: Coords, size :: Size, health :: Health }

-- Classes
class Entity e where
  move   :: e -> CoordX -> CoordY -> e

class Collidable e where
  collidesWithPlayer :: e -> e -> bool -- of zo
  onCollide          :: e -> e

class ShootingEntity e where
  shoot :: e -> e

-- Instances and commonalities
instance Entity Player where
  move p@Player { pos = pos } _ dy = p { pos = pos { y = clamp (-shipMaxY, shipMaxY) (y pos + dy) } }

instance Entity Enemy where
  move a@Astroid { pos = pos } dx dy = a { pos = pos + Coords dx dy}
  move a@Alien   { pos = pos } dx dy = a { pos = pos + Coords dx dy}

applyEnemyLogic :: GameState -> Enemy -> GameState
applyEnemyLogic gs Astroid {} = gs
applyEnemyLogic gs Alien {}   = gs