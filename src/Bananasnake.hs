module Bananasnake where

import Reactive.Banana
import Reactive.Banana.Frameworks

import Render

initialPosition = Position 3 3
initialFoodPosition = Position 5 5

newtype Direction = Direction Position deriving (Show, Eq)

up :: Direction
up =    Direction (Position 0 1)
right = Direction (Position 1 0)
down =  Direction (Position 0 (-1))
left =  Direction (Position (-1) 0)

ups :: Event t Char -> Event t ()
ups = fmap (const ()) . filterE (== 'w')

turnClockwise :: Direction -> Direction
turnClockwise (Direction (Position x y)) = Direction (Position (negate y) x)

turnAnticlockwise :: Direction -> Direction
turnAnticlockwise (Direction (Position x y)) = Direction (Position y (negate x))

direction :: Event t Char -> Event t Direction
direction = accumE up . fmap turn
  where
    turn :: Char -> Direction -> Direction
    turn 'a' = turnAnticlockwise
    turn 'd' = turnClockwise
    turn _ = id

sampledBy :: Event t a -> Event t b -> Event t b
sampledBy as bs = filterJust $ apply mapAToB as
  where
    --lastB :: Behavior t (Maybe b)
    lastB = stepper Nothing (fmap Just bs)

    --mapAToB :: Behavior t (a -> Maybe b)
    mapAToB = fmap const lastB

slidingWindow :: Int -> Event t a -> Event t [a]
slidingWindow size = accumE [] . fmap sw
  where
    sw :: a -> [a] -> [a]
    sw a as = take size (a : as)

snakeHeadPosition :: Event t Direction -> Event t Position
snakeHeadPosition = accumE initialPosition . fmap updatePos
  where updatePos (Direction dir) position = dir + position

foodPosition :: Event t HeadPosition -> Event t FoodPosition
foodPosition = accumE initialFoodPosition . fmap move
  where
    move headPos foodPos
      | headPos == foodPos = Position 1 1
      | otherwise = foodPos

snake :: Event t Char -> Behavior t Game
snake keyEvents = do
  let dir = direction keyEvents
  let directionOnUp = sampledBy (ups keyEvents) dir
  let headPosition = snakeHeadPosition directionOnUp
  let tailPositions = slidingWindow 6 headPosition
  Game <$> pure 10 <*> pure 10
    <*> stepper initialPosition headPosition
    <*> stepper [] tailPositions
    <*> stepper initialFoodPosition (foodPosition headPosition)
