module Render where

import Control.Monad (replicateM_)
import Data.Foldable (traverse_)
import System.Console.Terminal.Size (size, Window(..))

type X = Int
type Y = Int
data Position = Position X Y deriving (Show, Eq)

instance Num Position where
  (Position x y) + (Position x' y') = Position (x + x') (y + y')

type Width = Int
type Height = Int

type FoodPosition = Position
type HeadPosition = Position

data Game = Game {
  gameWidth :: Width,
  gameHeight :: Height,
  snakeHead :: HeadPosition,
  snakeTail :: [Position],
  food :: FoodPosition
}

renderCell :: Game -> Position -> Char
renderCell game pos
  | snakeHead game == pos = '@'
  | elem pos (snakeTail game) = '#'
  | food game == pos = '~'
  | otherwise = '·'

renderLine :: Game -> Y -> String
renderLine game y = map (\x -> renderCell game $ Position x y) [0..(gameWidth game - 1)]

render :: Game -> IO ()
render game = do
  let rows = map (renderLine game) [0..(gameHeight game - 1)]
  traverse_ putStrLn rows
  scrollToFit (length rows)

scrollToFit :: Int -> IO ()
scrollToFit numLines = do
  (Just (Window h _)) <- size
  replicateM_ (h - numLines) $ putStrLn ""
