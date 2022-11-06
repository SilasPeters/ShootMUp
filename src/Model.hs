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
data Player    = Player    { pos :: Coords, size :: Size, pace :: Speed }
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
  collidesWith :: (Entity m) => e -> m -> Bool -- probeert de eerste enemy of player te vinden (voeg de speler dus tijdelijk toe aan de lijst met enemies) waarmee deze entity collide. Moet entities van zijn eigen type uit de lijst filteren.
  onCollide    :: e -> GameState -> GameState -- wat deze entity doet als hij collide

class ShootingEntity e where
  shoot :: e -> GameState -> GameState

-- Instances and commonalities
instance Entity Player where
  move p@Player { pos = pos } dt _ dy = p { pos = pos { y = newClampedY } }
    where newClampedY = clamp (-shipMaxY, shipMaxY) (y pos + (dy * dt))
  getPos = pos
  getSize = size
  imgKey = const "player"

instance Entity Enemy where
  move e@Astroid { pos = pos } dt dx dy = e { pos = pos + Coords (dx * dt) (dy * dt)}
  move e@Alien   { pos = pos } dt dx dy = e { pos = pos + Coords (dx * dt) (dy * dt)}
  move e@Bullet  { pos = pos } dt dx dy = e { pos = pos + Coords (dx * dt) (dy * dt)}
  rotate e@Astroid { rotation = rotation } degree = e { rotation = rotation + degree}
  rotate e@Alien   { rotation = rotation } degree = e { rotation = rotation + degree}
  rotate e@Bullet  { rotation = rotation } degree = e { rotation = rotation + degree}
  getPos = pos
  getSize = size
  getRotation = rotation
  imgKey Alien   {} = "alien"
  imgKey Astroid {} = "astroid"
  imgKey Bullet  {} = "bullet"
  
instance Eq Enemy where
  x == y = x == y
  x /= y = x /= y
  
instance Collidable Player where
  collidesWith e o = let (Coords ex ey) = getPos e
                         (Coords ox oy) = getPos o
                         osize = getSize o
                     in (ex - shipWidth < ox + osize && ox - osize < ex + shipWidth) && (ey - shipHeigth < oy + osize && oy + osize < ey + shipHeigth || ey + shipHeigth > oy - osize && oy - osize > ey - shipHeigth)
  onCollide e gs = gs { alive = False }

instance Collidable Enemy where
  collidesWith e o = let (Coords ex ey) = getPos e
                         (Coords ox oy) = getPos o
                         esize = getSize e
                         osize = getSize o
                     in (ex - esize < ox + osize && ox - osize < ex + esize) && (ey - esize < oy + osize && oy + osize < ey + esize || ey + esize > oy - osize && oy - osize > ey - esize)
  onCollide e gs = gs {enemies = removeItem e (enemies gs)}

instance ShootingEntity Player where
  shoot p@Player {pos = pos} gs = gs { enemies = (Bullet {pos = pos { x = x pos + shipWidth + 10}, rotation = 0, size = 1, bulletspeed = 10, direction = (10, 0)}) : enemies gs}
  
instance ShootingEntity Enemy where
  shoot e@Alien {pos = Coords alienx alieny} gs@(GameState Player { pos = playerPos } keylist enemies time paused alive rng) = 
    let shipx = x playerPos 
        shipy = y playerPos in
    gs { enemies = (Bullet {pos = Coords alienx alieny, rotation = 0, size = 1, bulletspeed = 10, direction = (alienx -shipx, alieny - shipy)}) : enemies}
    
removeItem :: (Eq a) => a -> [a] -> [a]
removeItem _ []                 = []
removeItem x (y:ys) | x == y    = removeItem x ys
                    | otherwise = y : removeItem x ys -- todo: kan korter
-- Source: https://stackoverflow.com/questions/2097501/learning-haskell-how-to-remove-an-item-from-a-list-in-haskell