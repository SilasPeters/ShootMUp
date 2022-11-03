{-#LANGUAGE DuplicateRecordFields#-}

-- | This module contains the data types
--   which represent the state of the game
module Model where
--import Distribution.Simple (registrationPackageDB)
import Graphics.Gloss (Picture, Vector)
import SupportiveFunctions
import System.Random
import Data.List

shipMaxY = 220
shipWidth = 100
shipHeigth = 80

-- Our primitives
type Paused    = Bool
type Alive     = Bool
type Time      = Float
type CoordX    = Float
type CoordY    = Float
type Health    = Float
type Size      = Float
type Rotation  = Float
type Speed     = Float
type Direction = Vector

data Coords = Coords { x :: CoordX, y :: CoordY }
instance Num Coords where
  (+) (Coords x y) (Coords p q) = Coords (x + p) (y + q)
  (-) (Coords x y) (Coords p q) = Coords (x - p) (y - q)

times :: Coords -> Float -> Coords
(Coords x y) `times` n = Coords (x * n) (y * n)

-- New data types
data GameState = GameState { player :: Player, keyList :: [Char], enemies :: [Enemy], t :: Time, paused :: Paused, alive :: Alive, rng :: StdGen }
data Player    = Player    { pos :: Coords, pace :: Speed }
data Enemy     = Astroid   { pos :: Coords, rotation :: Rotation, size :: Size, speed :: Speed }
               | Alien     { pos :: Coords, rotation :: Rotation, size :: Size, speed :: Speed, health :: Health }
               | Bullet    { pos :: Coords, rotation :: Rotation, size :: Size, bulletspeed :: Speed, direction :: Direction}

-- Classes
class Entity e where
  move        :: e -> Time -> CoordX -> CoordY -> e
  rotate      :: e -> Rotation -> e
  getPos      :: e -> Coords
  getSize     :: e -> Size
  getRotation :: e -> Rotation
  imgKey      :: e -> String

class Collidable e where
  collidesWith :: (Entity m) => e -> [m] -> Maybe m -- probeert de eerste enemy of player te vinden (voeg de speler dus tijdelijk toe aan de lijst met enemies) waarmee deze entity collide. Moet entities van zijn eigen type uit de lijst filteren.
  onCollide    :: e -> GameState -> GameState -- wat deze entity doet als hij collide

class ShootingEntity e where
  shoot :: e -> GameState -> GameState

-- Instances and commonalities
instance Entity Player where
  move p@Player { pos = pos } dt _ dy = p { pos = pos { y = newClampedY } }
    where newClampedY = clamp (-shipMaxY, shipMaxY) (y pos + (dy * dt))
  getPos = pos
  imgKey = const "player"

instance Entity Enemy where
  move e@Astroid { pos = pos } dt dx dy = e { pos = pos + Coords (dx * dt) (dy * dt)}
  move e@Alien   { pos = pos } dt dx dy = e { pos = pos + Coords (dx * dt) (dy * dt)}
  rotate e@Astroid { rotation = rotation } degree = e { rotation = rotation + degree}
  rotate e@Alien   { rotation = rotation } degree = e { rotation = rotation + degree}
  getPos = pos
  getSize = size
  getRotation = rotation
  imgKey Alien   {} = "alien"
  imgKey Astroid {} = "astroid"
  imgKey Bullet  {} = "bullet"
  
instance Collidable Player where
  collidesWith e b = find (collides e) b where
                       collides e x = let (Coords ex ey) = getPos e
                                          (Coords bx by) = getPos x
                                          size = getSize e
                                      in bx - size < ex + shipWidth && (ey - shipHeigth < by + size && by + size < ey + shipHeigth || ey + shipHeigth > by - size && by - size > ey - shipHeigth)
  onCollide _ gs = gs { alive = False }

instance Collidable Enemy where
  collidesWith e b = find (collides e) b where
                       collides e x = let (Coords ex ey) = getPos e
                                          (Coords bx by) = getPos x
                                          esize = getSize e
                                          bsize = getSize x
                                      in bx + bsize < ex + esize && (ey - esize < by + bsize && by + bsize < ey + esize || ey + esize > by - bsize && by - bsize > ey - esize)
  onCollide _ gs = gs

instance ShootingEntity Player where
  shoot p@Player {pos = pos} gs = gs { enemies = (Bullet {pos = pos { x = x pos + shipWidth + 10}, rotation = 0, size = 1, bulletspeed = 10, direction = (10, 0)}) : enemies gs}
  
instance ShootingEntity Enemy where
  shoot e@Alien {pos = Coords alienx alieny} gs@(GameState Player { pos = playerPos } keylist enemies time paused alive rng) = 
    let shipx = x playerPos 
        shipy = y playerPos in
    gs { enemies = (Bullet {pos = Coords alienx alieny, rotation = 0, size = 1, bulletspeed = 10, direction = (alienx -shipx, alieny - shipy)}) : enemies}