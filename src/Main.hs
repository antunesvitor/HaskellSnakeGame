module Main where

import Graphics.Gloss
main :: IO ()

type Position = Int

topPos :: Position
topPos = 1

rightPos :: Position
rightPos = 2

bottomPos :: Position
bottomPos = 3

leftPos :: Position
leftPos = 4

data Node = Node {
    x :: Int
  , y :: Int
  , position :: Position
}

data Snake = Snake {
    snakeHead :: Node
  , snakeTail :: [Node]
}

data Resolution = Resolution {
  height :: Int
, width :: Int
}

constResolution :: Resolution
constResolution = Resolution 480 640

getScreenResolution :: (Int, Int)
getScreenResolution  = (width constResolution, height constResolution)

background :: Color
background = black

initialState :: Picture
initialState = pictures[
  walls, renderSnake startingSnake
 ]

walls :: Picture
walls = pictures 
  [
    topWall, bottoomWall, leftWall, rightWall
  ]
  where
    -- thickness = fromIntegral (snd constResolution `div` 10)
    screenWidth = fromIntegral (width constResolution)
    screenHeight = fromIntegral (height constResolution - 2 * 10)
    horizontalWall = rectangleSolid screenWidth 10
    verticalWall = rectangleSolid 10 screenHeight
    topWall = translate 0 (-235) $ color white $ horizontalWall
    bottoomWall = translate 0 235 $ color white $ horizontalWall
    leftWall = translate (-315) 0 $ color red $ verticalWall
    rightWall = translate 315 0 $ color red $ verticalWall


renderSnake :: Snake -> Picture
renderSnake snake = pictures $ (renderNode $ snakeHead snake):(renderedTail)
  where
    renderedTail = map renderNode $ snakeTail snake

renderNode :: Node -> Picture
renderNode (Node nodeX nodeY _) = translate intNodeX intNodeY $ color white $ rectangleSolid 10 10
  where
    intNodeX = fromIntegral nodeX
    intNodeY = fromIntegral nodeY

startingSnake :: Snake
startingSnake = Snake { snakeHead = startingHead,  snakeTail = startingTail startingHead }
  where
    startingHead = Node { x = 0, y = 0, position = 2}

startingTail :: Node -> [Node]
startingTail startingHead = startingTail' startingHead 3 []

startingTail' :: Node -> Int -> [Node] -> [Node]
startingTail' _ 0 initialTail = initialTail
startingTail' startingHead remaining [] = startingTail' startingHead (remaining - 1) [addNode startingHead]
startingTail' startingHead remaining (n:ns) = startingTail' startingHead (remaining - 1) (newNode:n:ns)
  where
    newNode = addNode n

addNode :: Node -> Node
addNode (Node nodeX nodeY direction)
  | direction == topPos = Node { x = nodeX, y = nodeY - 11, position = direction }
  | direction == rightPos = Node { x = nodeX + 11, y = nodeY, position = direction }
  | direction == bottomPos = Node { x = nodeX, y = nodeY + 11, position = direction }
  | otherwise = Node { x = nodeX - 11, y = nodeY, position = direction }



main = do
  display (InWindow "Nice Window" getScreenResolution (10, 10)) background initialState
 