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

turnClockwise :: Direction -> Direction
turnClockwise (Direction (Position x y)) = Direction (Position y (negate x))

turnAnticlockwise :: Direction -> Direction
turnAnticlockwise (Direction (Position x y)) = Direction (Position (negate y) x)

direction :: Event t Char -> Event t Direction
direction = accumE up . fmap turn
  where
    turn :: Char -> Direction -> Direction
    turn 'a' = turnAnticlockwise
    turn 'd' = turnClockwise
    turn _ = id

snakeHeadPosition :: Event t Direction -> Event t Position
snakeHeadPosition = accumE initialPosition . fmap updatePos
  where updatePos (Direction dir) position = dir + position

snake :: Event t Char -> Event t Game
snake keyEvents = do
  let dir = direction keyEvents
  let headPosition = snakeHeadPosition dir
  fmap updateGame headPosition
    where updateGame position = initialGame { snakeHead = position}
