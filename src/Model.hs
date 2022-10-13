-- | This module contains the data types
--   which represent the state of the game
module Model where
import Distribution.Simple (registrationPackageDB)

initialState :: GameState
initialState = GameState ShowNothing 0

-- Gamestate
newtype Paused = Paused Bool
newtype Time   = Time Int

-- General stuff
data GameState = GameState Player [Enemy] Time Paused
newtype CoordX = CoordX Float
newtype CoordY = CoordY Float
newtype Health = Health Float
newtype Size   = Size Float

-- Entities
newtype Player = Player CoordY
data Astroid   = Astroid Size (CoordX, CoordY)
data Alien     = Alien (CoordX, CoordY) Health

-- Classes
class Entity e where
  move    :: e -> CoordX -> CoordY -> e
  display :: e -> Picture

class Entity p => Player p where
  shoot :: p -> IO io

class Entity e => Enemy e where
  
class Enemy e => AdvancedEnemy e where
  shoot :: p -> IO io
  

-- instance Entity Player where
--   move (Player y) _ dy = Player (y + dy) -- afhankelijk van keyboard

-- instance Enemy Astroid where
--   move (Player y) _ dy = Player (y + dy) -- afhankelijk van keyboard