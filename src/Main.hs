module Main where

import Control.Monad (replicateM_, forever)
import Data.Foldable (traverse_)
import Data.List (intersperse)
import Reactive.Banana
import Reactive.Banana.Frameworks
import System.Console.Terminal.Size (size, Window(..))
import System.IO (BufferMode(..), hSetEcho, hSetBuffering, stdin)

type X = Int
type Y = Int
data Position = Position X Y deriving (Show, Eq)

type Width = Int
type Height = Int

data Game = Game {
  gameWidth :: Width,
  gameHeight :: Height,
  snakeHead :: Position,
  snakeTail :: [Position],
  food :: Position
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
  let foo = map (renderLine game) [0..(gameHeight game - 1)]
  traverse_ putStrLn foo

main :: IO ()
main = do
  --render $ Game 4 4 undefined undefined undefined
  turnOffInputBuffering
  (addKeyEvent, fireKey) <- newAddHandler
  network <- compile $ makeNetworkDescription addKeyEvent
  actuate network
  forever (getChar >>= fireKey)

makeNetworkDescription :: Frameworks t => AddHandler Char -> Moment t ()
makeNetworkDescription addKeyEvent = do
  keyEvents <- fromAddHandler addKeyEvent
  numberOfKeyPresses <- changes $ eventCounter keyEvents
  reactimate' $ fmap (putStrLn . show) <$> numberOfKeyPresses

eventCounter :: Event t a -> Behavior t Int
eventCounter = accumB 0 . fmap (const (+1))

turnOffInputBuffering :: IO ()
turnOffInputBuffering = do
  hSetEcho stdin False
  hSetBuffering stdin NoBuffering

scroll :: IO ()
scroll = do
  (Just (Window h _)) <- size
  replicateM_ h $ putStrLn ""
