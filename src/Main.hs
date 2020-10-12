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

data World = World {
    theSnake :: Snake
  , seed :: Node
} deriving Show

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
  width :: Int
 , height :: Int
}

-- The Number of the pixels that will separate a block from another
constNodeDistance :: Int
constNodeDistance = 1

-- Constant that defines the number of pixels a snake block will have
constBlockSize :: Int
constBlockSize = 10

-- Constant that defines the snake color
constSnakeColor :: Color
constSnakeColor = blue

-- Constant that defines the wall color
constWallColor :: Color
constWallColor = white

-- The snake velocity is the node distance plus the product of the block size and the number of blocks each step will take
constVelocity :: Int
constVelocity = 5

constResolution :: Resolution
constResolution = Resolution 640 360

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
    topWall, bottomWall, leftWall, rightWall
  ]
  where
    screenWidth = fromIntegral (width constResolution)
    screenHeight = fromIntegral (height constResolution)
    horizontalWall = rectangleSolid screenWidth (fromIntegral constBlockSize)
    verticalWall = rectangleSolid (fromIntegral constBlockSize) screenHeight
    -- verticalOffset = fromIntegral $ (height constResolution - constBlockSize) `div` 2
    verticalOffset = fromIntegral $ (height constResolution) `div` 2
    horizontalOffset = fromIntegral $ (width constResolution) `div` 2
    -- horizontalOffset = fromIntegral $ (width constResolution - constBlockSize) `div` 2
    topWall = translate 0 verticalOffset $ color constWallColor $ horizontalWall
    bottomWall = translate 0 (-verticalOffset) $ color constWallColor $ horizontalWall
    leftWall = translate (-horizontalOffset) 0 $ color constWallColor $ verticalWall
    rightWall = translate horizontalOffset 0 $ color constWallColor $ verticalWall

renderGame :: Snake -> Picture
renderGame snake = pictures [
  walls, renderSnake snake
  ]

renderSnake :: Snake -> Picture
renderSnake snake = pictures $ (renderNode $ snakeHead snake):(renderedTail)
  where
    renderedTail = map renderNode $ snakeTail snake

renderNode :: Node -> Picture
renderNode (Node nodeX nodeY _) = translate intNodeX intNodeY $ color constSnakeColor $ rectangleSolid floatBlockSize floatBlockSize
  where
    intNodeX = fromIntegral nodeX
    intNodeY = fromIntegral nodeY
    floatBlockSize = fromIntegral constBlockSize

startingWorld :: World
startingWorld = World startingSnake initialSeed

initialSeed :: Node
initialSeed = Node (-50) (-50) noDir

renderWorld :: World -> Picture
renderWorld world = pictures [
  walls, renderSnake $ theSnake world, renderNode $ seed world
  ]

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
  | direction' == topDir = Node { x = nodeX, y = nodeY - 10, direction = direction' }
  | direction' == rightDir = Node { x = nodeX + 10, y = nodeY, direction = direction' }
  | direction' == bottomDir = Node { x = nodeX, y = nodeY + 10, direction = direction' }
  | otherwise = Node { x = nodeX - 10, y = nodeY, direction = direction' }

stepWorld :: World -> Direction -> World
stepWorld world dir 
  | hasCollided $ World movedSnake currentSeed = World newSnake $ newSeed currentSeed
  | otherwise = World movedSnake $ seed world
  where
    movedSnake = moveSnake (theSnake world) dir
    currentSeed = seed world
    newSnake = insertNewNode movedSnake

insertNewNode :: Snake -> Snake
insertNewNode snake = Snake { snakeHead = snakeHead snake, snakeTail = incrementTail $ snakeTail snake, velocity = velocity snake }

incrementTail :: [Node] -> [Node]
incrementTail [] = []
incrementTail [n] = [ n, addNode n ]
incrementTail (n:ns) = n:(incrementTail ns)

hasCollided :: World -> Bool
hasCollided world = pythagoreanDistanceSquared <= radioSquared 
  where 
    xSnake = x $ snakeHead $ theSnake world 
    ySnake = y $ snakeHead $ theSnake world 
    xSeed = x $ seed world
    ySeed = y $ seed world
    square = 2
    pythagoreanDistanceSquared = (xSnake - xSeed) ^ square + (ySnake - ySeed) ^ square
    radioSquared = (constBlockSize `div` 2) ^ square

newSeed :: Node -> Node
newSeed oldSeed = Node (- x oldSeed) (- y oldSeed) noDir

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

oppositeDirection :: Direction -> Direction
oppositeDirection dir 
  | dir == topDir = bottomDir
  | dir == rightDir = leftDir
  | dir == bottomDir = topDir
  | dir == leftDir = rightDir
  | otherwise = noDir 

keepOrChangeDirection :: Direction -> Direction -> Direction
keepOrChangeDirection current new
  | current == oppositeDirection new = current
  | otherwise = new

moveTail :: [Node] -> Node -> [Node]
moveTail [] _ = []
moveTail (first:ys) targetNode = targetNode:(moveTail ys first)

update :: ViewPort -> Float -> Snake -> Snake
update _ _ snake = moveSnake snake topDir

updatePlay :: Float -> Snake -> Snake
updatePlay _ snake = moveSnake snake noDir

updatePlayWorld :: Float -> World -> World
updatePlayWorld _ world = stepWorld world noDir

handleKeys :: Event -> Snake -> Snake
handleKeys (EventKey (SpecialKey key) _ _ _) (Snake h t v)
  | key == KeyUp = Snake { snakeHead = Node { x = x h, y = y h, direction = keepOrChangeDirection currentDirection topDir }, snakeTail = t, velocity = v}
  | key == KeyRight = Snake { snakeHead = Node { x = x h, y = y h, direction = keepOrChangeDirection currentDirection rightDir }, snakeTail = t, velocity = v }
  | key == KeyDown = Snake { snakeHead = Node { x = x h, y = y h, direction = keepOrChangeDirection currentDirection bottomDir }, snakeTail = t, velocity = v }
  | key == KeyLeft = Snake { snakeHead = Node { x = x h, y = y h, direction = keepOrChangeDirection currentDirection leftDir }, snakeTail = t, velocity = v }
  | otherwise = Snake h t v
  where
    currentDirection = direction h
handleKeys _ (Snake h t v) = Snake h t v


handleKeysWorld :: Event -> World -> World
handleKeysWorld (EventKey (SpecialKey key) _ _ _) (World (Snake h t v) s)
  | key == KeyUp = World (Snake { snakeHead = Node { x = x h, y = y h, direction = keepOrChangeDirection currentDirection topDir }, snakeTail = t, velocity = v}) s
  | key == KeyRight = World (Snake { snakeHead = Node { x = x h, y = y h, direction = keepOrChangeDirection currentDirection rightDir }, snakeTail = t, velocity = v }) s
  | key == KeyDown = World (Snake { snakeHead = Node { x = x h, y = y h, direction = keepOrChangeDirection currentDirection bottomDir }, snakeTail = t, velocity = v }) s
  | key == KeyLeft = World (Snake { snakeHead = Node { x = x h, y = y h, direction = keepOrChangeDirection currentDirection leftDir }, snakeTail = t, velocity = v }) s
  | otherwise = World (Snake h t v) s
  where
    currentDirection = direction h
handleKeysWorld _ (World (Snake h t v) s ) = World (Snake h t v) s

main = do
  -- display window black $ renderSnake startingSnake
  -- simulate window black 2 startingSnake renderGame update
  -- play window black 60 startingSnake renderSnake handleKeys updatePlay
  play window black 60 startingWorld renderWorld handleKeysWorld updatePlayWorld
  where
    window = (InWindow "Snake Game" getScreenResolution (10, 10))