module Bananasnake where

import Reactive.Banana
import Reactive.Banana.Frameworks

import Render

initialPosition = (Position 3 3)

initialGame = Game 10 10 (Position 5 5) [] (Position 4 4)

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

combine :: (a -> b -> c) -> Event t a -> Event t b -> Behavior t (Maybe c)
combine f ea eb = fmap (liftA2 f) ba <*> bb
  where
    ba = stepper Nothing (fmap Just ea)
    bb = stepper Nothing (fmap Just eb)

snake :: Event t Char -> Behavior t (Maybe Game)
snake keyEvents = do
  let dir = direction keyEvents
  let directionOnUp = sampledBy (ups keyEvents) dir
  let headPosition = snakeHeadPosition directionOnUp
  let tailPositions = slidingWindow 6 headPosition
  combine blah headPosition tailPositions
  where
    blah :: Position -> [Position] -> Game
    blah headPos tailPos = initialGame {
      snakeHead = headPos,
      snakeTail = tailPos
    }
