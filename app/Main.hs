module Main where

import SnakeGameWorld
import Graphics.Gloss

main :: IO ()

background :: Color
background = makeColor 200 200 200 0

main = do
  -- print horizontalOffset
  play window background 30 startingWorld renderWorld handleKeysWorld updatePlayWorld
  where
    window = (InWindow "Snake Game" (320, 568) (10, 10))