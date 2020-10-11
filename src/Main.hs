module Main where

import Graphics.Gloss
import Graphics.Gloss.Data.ViewPort
import Graphics.Gloss.Interface.IO.Interact
main :: IO ()

type Direction = Int

noDir :: Direction
noDir = 0

topDir :: Direction
topDir = 1

rightDir :: Direction
rightDir = 2

bottomDir :: Direction
bottomDir = 3

leftDir :: Direction
leftDir = 4

data Node = Node {
    x :: Int
  , y :: Int
  , direction :: Direction -- direção que cada bloco 
} deriving Show

data Snake = Snake {
    snakeHead :: Node
  , snakeTail :: [Node]
  , velocity :: Int 
} deriving Show

data Resolution = Resolution {
  height :: Int
, width :: Int
}

-- The Number of the pixels that will separate a block from another
constNodeDistance :: Int
constNodeDistance = 1

-- Constant that defines the number of pixels a snake block will have
constSnakeBlockSize :: Int
constSnakeBlockSize = 10

-- Constant that defines the snake color
constSnakeColor :: Color
constSnakeColor = blue

-- Constant that defines the wall color
constWallColor :: Color
constWallColor = white

-- The snake velocity is the node distance plus the product of the block size and the number of blocks each step will take
constVelocity :: Int
constVelocity = (1 * constSnakeBlockSize) + constNodeDistance

constResolution :: Resolution
constResolution = Resolution 360 640

getScreenResolution :: (Int, Int)
getScreenResolution  = (width constResolution, height constResolution)

background :: Color
background = black

initialState :: Picture
initialState = pictures [
  walls, renderSnake startingSnake
 ]

walls :: Picture
walls = pictures [
    topWall, bottoomWall, leftWall, rightWall
  ]
  where
    -- thickness = fromIntegral (snd constResolution `div` 10)
    screenWidth = fromIntegral (width constResolution)
    screenHeight = fromIntegral (height constResolution - 2 * 10)
    horizontalWall = rectangleSolid screenWidth 10
    verticalWall = rectangleSolid 10 screenHeight
    topWall = translate 0 (-235) $ color constWallColor $ horizontalWall
    bottoomWall = translate 0 235 $ color constWallColor $ horizontalWall
    leftWall = translate (-315) 0 $ color constWallColor $ verticalWall
    rightWall = translate 315 0 $ color constWallColor $ verticalWall

renderGame :: Snake -> Picture
renderGame snake = pictures [
  walls, renderSnake snake
  ]

renderSnake :: Snake -> Picture
renderSnake snake = pictures $ (renderNode $ snakeHead snake):(renderedTail)
  where
    renderedTail = map renderNode $ snakeTail snake

renderNode :: Node -> Picture
renderNode (Node nodeX nodeY _) = translate intNodeX intNodeY $ color constSnakeColor $ rectangleSolid 10 10
  where
    intNodeX = fromIntegral nodeX
    intNodeY = fromIntegral nodeY

startingSnake :: Snake
startingSnake = Snake { snakeHead = startingHead, snakeTail = startingTail startingHead, velocity = constVelocity }
  where
    startingHead = Node { x = 0, y = 0, direction = topDir}

startingTail :: Node -> [Node]
startingTail startingHead = startingTail' startingHead 5 []

startingTail' :: Node -> Int -> [Node] -> [Node]
startingTail' _ 0 initialTail = initialTail
startingTail' startingHead remaining [] = startingTail' startingHead (remaining - 1) [addNode startingHead]
startingTail' startingHead remaining (n:ns) = startingTail' startingHead (remaining - 1) (newNode:n:ns)
  where
    newNode = addNode n

addNode :: Node -> Node
addNode (Node nodeX nodeY direction')
  | direction' == topDir = Node { x = nodeX, y = nodeY - 11, direction = direction' }
  | direction' == rightDir = Node { x = nodeX + 11, y = nodeY, direction = direction' }
  | direction' == bottomDir = Node { x = nodeX, y = nodeY + 11, direction = direction' }
  | otherwise = Node { x = nodeX - 11, y = nodeY, direction = direction' }

-- Given a snake model and a direction move snake one or more blocks in the direction
moveSnake :: Snake -> Direction -> Snake
moveSnake (Snake h t vel) dir = Snake newHead newTail vel
  where 
    currentDir = direction h
    newHead =  moveNode h vel $ evalDirection dir currentDir
    newTail = moveTail t h

-- moves a Node to a given direction in a given velocity (velocity = blocks)
moveNode :: Node -> Int -> Direction -> Node
moveNode (Node nodeX nodeY _) vel newDirection 
  | newDirection == topDir = Node { x = nodeX, y = nodeY + vel, direction = topDir}
  | newDirection == rightDir = Node { x = nodeX + vel, y = nodeY, direction = rightDir}
  | newDirection == bottomDir = Node { x = nodeX, y = nodeY - vel, direction = bottomDir}
  | otherwise  = Node { x = nodeX - vel, y = nodeY, direction = leftDir}

-- Verify if direction is null (noDir or 0) or a valid direction
-- keeping the new current direction if null or changing if not
evalDirection :: Direction -> Direction -> Direction
evalDirection new current 
  | new == noDir = current
  | otherwise = new

moveTail :: [Node] -> Node -> [Node]
moveTail [] _ = []
moveTail (first:ys) targetNode = targetNode:(moveTail ys first)

update :: ViewPort -> Float -> Snake -> Snake
update _ _ snake = moveSnake snake topDir

updatePlay :: Float -> Snake -> Snake
updatePlay _ snake = moveSnake snake noDir

handleKeys :: Event -> Snake -> Snake
handleKeys (EventKey (SpecialKey key) _ _ _) (Snake h t v)
  | key == KeyUp = Snake { snakeHead = Node { x = x h, y = y h, direction = topDir }, snakeTail = t, velocity = v}
  | key == KeyRight = Snake { snakeHead = Node { x = x h, y = y h, direction = rightDir }, snakeTail = t, velocity = v }
  | key == KeyDown = Snake { snakeHead = Node { x = x h, y = y h, direction = bottomDir }, snakeTail = t, velocity = v }
  | key == KeyLeft = Snake { snakeHead = Node { x = x h, y = y h, direction = leftDir }, snakeTail = t, velocity = v }
  | otherwise = Snake h t v
handleKeys _ (Snake h t v) = Snake h t v

main = do
  -- simulate window black 2 startingSnake renderGame update
  play window black 10 startingSnake renderGame handleKeys updatePlay
  where
    window = (InWindow "Snake Game" getScreenResolution (10, 10))
 