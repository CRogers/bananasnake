module Main where

import System.Console.Terminal.Size (size, Window(..))

main :: IO ()
main = do
  (Just (Window h w)) <- size
  putStrLn $ show h
  putStrLn $ show w
