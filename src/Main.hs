module Main where

import SnakeGameWorld
import Graphics.Gloss

main :: IO ()

background :: Color
background = black

main = do
  -- print horizontalOffset
  play window background 60 startingWorld renderWorld handleKeysWorld updatePlayWorld
  where
    window = (InWindow "Snake Game" (640, 360) (10, 10))