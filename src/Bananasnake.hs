module Bananasnake where

import Reactive.Banana
import Reactive.Banana.Frameworks

import Render

initialPosition = (Position 2 2)

initialGame = Game 10 10 (Position 5 5) [] (Position 4 4)

newtype Direction = Direction Position

up :: Direction
up = Direction (Position 0 1)

turnClockwise :: Direction -> Direction
turnClockwise (Direction (Position x y)) = Direction (Position (negate y) (negate x))

direction :: Event t Char -> Behavior t Direction
direction = accumB up . fmap (const turnClockwise)

snakeHeadPosition :: Behavior t Direction -> Behavior t Position
snakeHeadPosition bDir = fmap updatePos bDir <*> pure initialPosition
  where updatePos (Direction dir) position = dir + position

snake :: Event t Char -> Behavior t Game
snake keyEvents = do
  let dir = direction keyEvents
  let headPosition = snakeHeadPosition dir
  fmap updateGame headPosition
    where updateGame position = initialGame { snakeHead = position}
