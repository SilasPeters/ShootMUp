{-#LANGUAGE DuplicateRecordFields#-}

-- | This module contains the data types
--   which represent the state of the game
module Model where
--import Distribution.Simple (registrationPackageDB)
import Graphics.Gloss (Picture, Vector)
import SupportiveFunctions
import System.Random

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
data GameState = GameState { player :: Player, keyList :: [Char], enemies :: [Enemy], t :: Time, paused :: Paused, alive :: Alive, bullets :: [Bullet], rng :: StdGen }
data Player    = Player    { pos :: Coords, pace :: Speed }
data Enemy     = Astroid   { pos :: Coords, rotation :: Rotation, size :: Size, speed :: Speed } | Alien { pos :: Coords, rotation :: Rotation, size :: Size, speed :: Speed, health :: Health }
data Bullet    = Bullet    { pos :: Coords, bulletspeed :: Speed, direction :: Direction}

-- Classes
class Entity e where
  move   :: e -> Time -> CoordX -> CoordY -> e
  rotate :: e -> Rotation -> e
  getPos :: e -> Coords
  getSize :: e -> Size
  imgKey :: e -> String

class Collidable e where
  collidesWithPlayer :: (Entity m) => e -> m -> Bool -- of zo
  onCollide          :: e -> Bullet -> Bool

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
  imgKey Alien   {} = "alien"
  imgKey Astroid {} = "astroid"
  
instance Entity Bullet where
  getPos = pos
  
instance Collidable Player where
  collidesWithPlayer Player { pos = Coords shipx shipy } e =
    let ex = x (getPos e)
        ey = y (getPos e)
        size = getSize e in
    ex - size < shipx + shipWidth && (shipy - shipHeigth < ey + size && ey + size < shipy + shipHeigth || shipy + shipHeigth > ey - size && ey - size > shipy - shipHeigth)
  onCollide Player { pos = Coords shipx shipy } Bullet { pos = Coords bx by} = bx < shipx + shipWidth && (shipy - shipHeigth < by && by < shipy + shipHeigth || shipy + shipHeigth > by && by > shipy - shipHeigth)

instance Collidable Enemy where
  onCollide Astroid { pos = Coords ax ay, size = size } Bullet { pos = Coords bx by} = bx < ax + size && (ay - size < by && by < ay + size || ay + size > by && by > ay - size)
  onCollide Alien { pos = Coords ax ay, size = size } Bullet { pos = Coords bx by} = bx < ax + size && (ay - size < by && by < ay + size || ay + size > by && by > ay - size)

instance ShootingEntity Player where
  shoot p@Player {pos = pos} gs = gs { bullets = (Bullet {pos = pos, bulletspeed = 10, direction = (10, 0)}) : bullets gs}
  
instance ShootingEntity Enemy where
  shoot e@Alien {pos = Coords alienx alieny} gs@(GameState Player { pos = playerPos } keylist enemies time paused alive bullets rng) = 
    let shipx = x playerPos 
        shipy = y playerPos in
    gs { bullets = (Bullet {pos = Coords alienx alieny, bulletspeed = 10, direction = (alienx -shipx, alieny - shipy)}) : bullets}