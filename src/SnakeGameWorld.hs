module SnakeGameWorld
  (
    Direction
  , Resolution
  , World
  , startingWorld
  , renderWorld
  , updatePlayWorld
  , handleKeysWorld
  , horizontalOffset
  , verticalOffset
  ) where
  
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Interact

data Direction = North | East | South | West | NoDir deriving (Eq, Show)

data World = World {
    theSnake :: Snake
  , seed :: Node
} deriving Show

-- data Configuration = Configuration {
--   resolution :: Resolution
--  ,backgroundColor :: Color
--  ,snakeColor :: Color

-- }

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
constSnakeColor = black

-- Constant that defines the snake color
constSeedColor :: Color
constSeedColor = red

-- Constant that defines the wall color
constWallColor :: Color
constWallColor = black

-- The snake velocity is the node distance plus the product of the block size and the number of blocks each step will take
constVelocity :: Int
constVelocity = 5

constResolution :: Resolution
constResolution = Resolution 320 568

verticalOffset :: Float
verticalOffset = (fromIntegral $ height constResolution) / 2

horizontalOffset :: Float
horizontalOffset = (fromIntegral $ width constResolution) / 2

buildWorld :: Snake -> Direction -> Node -> World
buildWorld (Snake h t v) dir seed' = World { theSnake = Snake { snakeHead = Node { x = x h, y = y h, direction = dir}, snakeTail = t, velocity = v}, seed = seed'}

walls :: Picture
walls = pictures [
    topWall, bottomWall, leftWall, rightWall
  ]
  where
    screenWidth = fromIntegral (width constResolution)
    screenHeight = fromIntegral (height constResolution)
    horizontalWall = rectangleSolid screenWidth (fromIntegral constBlockSize)
    verticalWall = rectangleSolid (fromIntegral constBlockSize) screenHeight
    topWall = translate 0 verticalOffset $ color constWallColor $ horizontalWall
    bottomWall = translate 0 (-verticalOffset) $ color constWallColor $ horizontalWall
    leftWall = translate (-horizontalOffset) 0 $ color constWallColor $ verticalWall
    rightWall = translate horizontalOffset 0 $ color constWallColor $ verticalWall

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

renderSeed :: Node -> Picture
renderSeed (Node nodeX nodeY _) = translate intNodeX intNodeY $ color constSeedColor $ rectangleSolid floatBlockSize floatBlockSize
  where
    intNodeX = fromIntegral nodeX
    intNodeY = fromIntegral nodeY
    floatBlockSize = fromIntegral constBlockSize

startingWorld :: World
startingWorld = World startingSnake initialSeed

initialSeed :: Node
initialSeed = Node (-50) (-50) NoDir

renderWorld :: World -> Picture
renderWorld world = pictures [
  walls, renderSnake $ theSnake world, renderSeed $ seed world
  ]

startingSnake :: Snake
startingSnake = Snake { snakeHead = startingHead, snakeTail = startingTail startingHead, velocity = constVelocity }
  where
    startingHead = Node { x = 0, y = 0, direction = North}

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
  | direction' == North = Node { x = nodeX, y = nodeY - 10, direction = direction' }
  | direction' == East = Node { x = nodeX + 10, y = nodeY, direction = direction' }
  | direction' == South = Node { x = nodeX, y = nodeY + 10, direction = direction' }
  | otherwise = Node { x = nodeX - 10, y = nodeY, direction = direction' }

stepWorld :: World -> Direction -> World
stepWorld world dir 
  | hasCollided $ World movedSnake currentSeed = World newSnake $ newSeed currentSeed
  | wallCollided $ World movedSnake currentSeed = startingWorld               -- Como não tem um "Game over" implementado ele simplesmente reseta
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
newSeed oldSeed = Node (- x oldSeed) (- y oldSeed) NoDir

-- Given a snake model and a direction move snake one or more blocks in the direction
moveSnake :: Snake -> Direction -> Snake
moveSnake (Snake h t vel) dir = Snake newHead newTail vel
  where 
    currentDir = direction h
    newHead =  moveNode h vel $ evalDirection dir currentDir
    newTail = moveTail t h

-- moves a Node to a given direction in a given velocity (velocity = blocks)
moveNode :: Node -> Int -> Direction -> Node
moveNode _ _ NoDir = error "moveNode function needs to  have a valid direction"
moveNode (Node nodeX nodeY _) vel North = Node { x = nodeX, y = nodeY + vel, direction = North}
moveNode (Node nodeX nodeY _) vel East  = Node { x = nodeX + vel, y = nodeY, direction = East}
moveNode (Node nodeX nodeY _) vel South = Node { x = nodeX, y = nodeY - vel, direction = South}
moveNode (Node nodeX nodeY _) vel West  = Node { x = nodeX - vel, y = nodeY, direction = West }

-- Verify if direction is null (noDir or 0) or a valid direction
-- keeping the new current direction if null or changing if not
evalDirection :: Direction -> Direction -> Direction
evalDirection NoDir current = current
evalDirection new _ = new

oppositeDirection :: Direction -> Direction
oppositeDirection North = South
oppositeDirection East = West
oppositeDirection South = North
oppositeDirection West = East
oppositeDirection NoDir = NoDir

keepOrChangeDirection :: Direction -> Direction -> Direction
keepOrChangeDirection current new
  | current == oppositeDirection new = current
  | otherwise = new

moveTail :: [Node] -> Node -> [Node]
moveTail [] _ = []
moveTail (first:ys) targetNode = targetNode:(moveTail ys first)

wallCollided :: World -> Bool
wallCollided (World (Snake h _ _) _) = bottomCollision || topCollision || rightCollision || leftCollision
  where
    xHead = fromIntegral $ x h
    yHead = fromIntegral $ y h 
    blockColisionRange = fromIntegral $ constBlockSize `div` 2
    topLimit = verticalOffset - blockColisionRange
    rightLimit = horizontalOffset - blockColisionRange
    bottomLimit = (-verticalOffset) + blockColisionRange
    leftLimit = (-horizontalOffset) + blockColisionRange
    topCollision = yHead > topLimit
    rightCollision = xHead > rightLimit
    bottomCollision = yHead < bottomLimit
    leftCollision = xHead < leftLimit

updatePlayWorld :: Float -> World -> World
updatePlayWorld _ world = stepWorld world NoDir

handleKeysWorld :: Event -> World -> World
handleKeysWorld (EventKey (SpecialKey key) _ _ _) (World snake seed')
  | key == KeyUp =  buildWorld snake (keepOrChangeDirection currentDirection North) seed'
  | key == KeyRight = buildWorld snake (keepOrChangeDirection currentDirection East) seed'
  | key == KeyDown = buildWorld snake (keepOrChangeDirection currentDirection South) seed'
  | key == KeyLeft = buildWorld snake (keepOrChangeDirection currentDirection West) seed'
  | otherwise = buildWorld snake (keepOrChangeDirection currentDirection NoDir) seed'
  where
    currentDirection = direction (snakeHead snake)
handleKeysWorld _ (World (Snake h t v) s ) = World (Snake h t v) s