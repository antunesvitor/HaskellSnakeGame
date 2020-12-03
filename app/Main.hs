module Main where

import SnakeGameWorld
import Graphics.Gloss
-- import Letters
import System.Random

background :: Color
background = makeColor 200 200 200 0

constResolution :: Resolution
constResolution = Resolution 320 568

getResolution :: Resolution -> (Int, Int)
getResolution r = (width r, height r) 
  
main :: IO ()
main = do
  let gen = mkStdGen 123412341234
  let (initialGen, _) = randomR (1, 5000) gen
  play window background 30 (startingWorld initialGen) renderWorld handleKeysWorld updatePlayWorld
  -- display window white muendo
  where
    window = InWindow "Snake Game" resolution (200, 50)
    resolution = getResolution constResolution