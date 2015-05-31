module Main where

import Control.Monad (forever)
import Reactive.Banana
import Reactive.Banana.Frameworks
import System.IO (BufferMode(..), hSetEcho, hSetBuffering, stdin)

import Render
import Bananasnake

main :: IO ()
main = do
  turnOffInputBuffering
  (addKeyEvent, fireKey) <- newAddHandler
  network <- compile $ makeNetworkDescription addKeyEvent
  actuate network
  forever (getChar >>= fireKey)

maybeToIO :: (a -> IO ()) -> Maybe a -> IO ()
maybeToIO _ Nothing = return ()
maybeToIO f (Just a) = f a

makeNetworkDescription :: Frameworks t => AddHandler Char -> Moment t ()
makeNetworkDescription addKeyEvent = do
  keyEvents <- fromAddHandler addKeyEvent
  game <- changes $ snake keyEvents
  reactimate' $ fmap (fmap (maybeToIO render)) game

turnOffInputBuffering :: IO ()
turnOffInputBuffering = do
  hSetEcho stdin False
  hSetBuffering stdin NoBuffering
