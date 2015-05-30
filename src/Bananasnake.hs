module Bananasnake where

import Reactive.Banana

import Render

initialPosition = (Position 2 2)

initialGame = Game 10 10 (Position 5 5) [] (Position 4 4)

snakeHeadPosition :: Event t Char -> Behavior t Position
snakeHeadPosition = accumB initialPosition . fmap updatePos
  where updatePos key position = position + Position 0 1

snake :: Event t Char -> Behavior t Game
snake keyEvents = fmap updateGame $ snakeHeadPosition keyEvents
  where updateGame position = initialGame { snakeHead = position}
