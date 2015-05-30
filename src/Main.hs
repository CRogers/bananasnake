module Main where

import Control.Monad (replicateM_, forever)
import Reactive.Banana
import Reactive.Banana.Frameworks
import System.Console.Terminal.Size (size, Window(..))
import System.IO (BufferMode(..), hSetEcho, hSetBuffering, stdin)

main :: IO ()
main = do
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
