{-# LANGUAGE TemplateHaskell, FlexibleContexts #-}

module Racing where

import Control.Monad (guard)
import Data.Maybe (fromMaybe)

import Data.Sequence (Seq, ViewL(..), ViewR(..), (<|), (|>))
import qualified Data.Sequence as S
import Lens.Micro.TH (makeLenses)
import Lens.Micro ((&), (.~), (%~), (^.))
import Linear.V2 (V2(..), _x, _y)
import System.Random (Random(..), newStdGen)

-- Types

data Game = Game 
  { _car      :: Car            -- ^ indicates current position of car
  , _col      :: Column         -- ^ current column of car
  , _enemies  :: Seq Car        -- ^ location of the enemies
  , _next     :: Stream Column  -- ^ infinite list of random next enemy initial column
  , _timer    :: Int            -- ^ determines appearance of next enemy
  , _dead     :: Bool           -- ^ game over flag
  , _paused   :: Bool           -- ^ paused flag
  , _score    :: Int            -- ^ score
  , _frozen   :: Bool           -- ^ freeze to disallow duplicate turns between time steps
  } deriving (Show)

data Column = L | R deriving (Show, Enum, Bounded)
data Stream a = a :| Stream a
  deriving (Show)

type Coord = V2 Int
type Car = Seq Coord 

makeLenses ''Game

-- Constants

height, width, enemyFreq :: Int
height = 30
width  = 6
enemyFreq = 8

rightEnemy, leftEnemy, leftCar, rightCar :: Car
leftEnemy = S.fromList [  V2 1 (height - 1)
                        , V2 0 (height - 2)
                        , V2 1 (height - 2)
                        , V2 2 (height - 2)
                        , V2 1 (height - 3)
                        , V2 0 (height - 4)
                        , V2 2 (height - 4)]

rightEnemy = S.fromList [ V2 4 (height - 1)
                       , V2 3 (height - 2)
                       , V2 4 (height - 2)
                       , V2 5 (height - 2)
                       , V2 4 (height - 3)
                       , V2 3 (height - 4)
                       , V2 5 (height - 4)]

leftCar = S.fromList [ V2 1 5
                     , V2 0 4
                     , V2 1 4
                     , V2 2 4
                     , V2 1 3
                     , V2 0 2
                     , V2 2 2 ]

rightCar = S.fromList [ V2 4 5
                      , V2 3 4
                      , V2 4 4
                      , V2 5 4
                      , V2 4 3
                      , V2 3 2
                      , V2 5 2 ]

-- Functions

dec = \x -> x - 1

-- | Step forward in time
step :: Game -> Game
step g = fromMaybe g $ do
  guard (not $ g ^. paused || g ^. dead)
  let g' = g & frozen .~ False
  return . fromMaybe (move g') $ die g'

-- | Possibly die if car crashes
die :: Game -> Maybe Game
die g = do
  guard (or $ elem <$> g ^. car  <*> g ^. enemies)
  return $ g & dead .~ True

-- | Move all enemies one step closer
move :: Game -> Game
move g = case g ^. timer of
          0 -> (addEnemy g) & (timer .~ enemyFreq) 
                          & (car .~ steer g)
                          & (score %~ (+10))

          _ -> g & (enemies %~ shiftAll) 
                            & (timer %~ dec) 
                            & (car .~ steer g)
                            & (score %~ (+10))

-- | Adds new enemy to game
addEnemy :: Game -> Game
addEnemy g = 
    g & (enemies .~ newEnemy (g ^. enemies) col) 
      & (next .~ s) 
    where (col :| s) = g ^. next

newEnemy :: Seq Car -> Column -> Seq Car
newEnemy s L = leftEnemy  <| s 
newEnemy s R = rightEnemy <| s

-- | Shifts all other cars downwards.
shiftAll :: Seq Car -> Seq Car
shiftAll = fmap $ shiftCar . S.viewr

-- | Shifts a single car downwards
shiftCar :: ViewR Coord -> Car
shiftCar EmptyR = S.empty
shiftCar (s :> a) = 
  case a ^. _y of
    0 -> shiftCar $ S.viewr s 
    _ -> (shiftCar $ S.viewr s) |> (a & _y %~ dec)  

-- Implicitly unpauses yet freezes game
turn :: Column -> Game -> Game
turn c g =
  if g ^. frozen
     then g
     else g & col .~ c
            & paused .~ False
            & frozen .~ True

steer :: Game -> Car
steer g = case g ^. col of
  L -> leftCar
  R -> rightCar

-- | Initialize a paused game with random food location
initGame :: IO Game
initGame = do
  nmes <- fromList . randomRs (L, R) <$> newStdGen
  let g  = Game { _car      = leftCar
            , _col      = L
            , _enemies  = S.empty
            , _next     = nmes
            , _timer    = enemyFreq
            , _score     = 0
            , _dead = False, _paused = True , _frozen = False }
  return g

instance Random Column where
    random g = case randomR (0,1) g of
                 (r, g') -> (toEnum r, g')
    randomR (a,b) g = case randomR (fromEnum a, fromEnum b) g of
                        (r, g') -> (toEnum r, g')

fromList :: [a] -> Stream a
fromList = foldr (:|) (error "Streams must be infinite")