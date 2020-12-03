module SnakeGameWorld
  (
    Direction(..)
  , Resolution(Resolution)
  , World
  , Node(..)
  , width
  , height
  , startingWorld
  , renderWorld
  , renderNode
  , updatePlayWorld
  , handleKeysWorld
  , horizontalOffset
  , verticalOffset
  ) where
  
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Interact
import Letters
import System.Random



data Direction = North | East | South | West | NoDir deriving (Eq, Show)

data GameState = Menu | Gaming | Paused | GameOver deriving (Eq, Show)

data World = World {
    theSnake :: Snake
  , seed :: Node
  , state :: GameState
  , randomnessSeed :: Int
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
constVelocity = 10

constResolution :: Resolution
constResolution = Resolution 320 568

verticalOffset :: Float
verticalOffset = (fromIntegral $ height constResolution) / 2

horizontalOffset :: Float
horizontalOffset = (fromIntegral $ width constResolution) / 2

buildWorld :: Snake -> Direction -> Node -> GameState -> Int -> World
buildWorld (Snake h t v) dir seed' sta gen = World { theSnake = Snake { snakeHead = Node { x = x h, y = y h, direction = dir}, snakeTail = t, velocity = v}, seed = seed', state = sta, randomnessSeed = gen} 

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
renderSnake snake = pictures $ (renderSnakeHead $ snakeHead snake) : renderedTail
  where
    renderedTail = map renderNode $ snakeTail snake

renderSnakeHead :: Node -> Picture
renderSnakeHead (Node x' y' dir) = translate intx inty $ renderSnakeHead' dir
  where 
    intx = fromIntegral x'
    inty = fromIntegral y'

renderSnakeHead' :: Direction -> Picture
renderSnakeHead' North = rotate 270 $ translate (-5) (5) $ pictures [headShape, eyes]
renderSnakeHead' East = translate (-5) (5) $ pictures [headShape, eyes]
renderSnakeHead' South = rotate 90 $ translate (-5) (5) $ pictures [headShape, eyes]
renderSnakeHead' West = rotate 180 $ translate (-5) (5) $ pictures [headShape, eyes]
renderSnakeHead' NoDir = error "A cabeça da cobra deve ter uma direação"

headShape :: Picture
headShape = Polygon [(0,0),(6,0), (7,-1), (8,-2),(9,-3),(10,-4),(10,-6),(9,-7),(8,-8),(7,-9),(6,-10), (0,-10)]

eyes :: Picture
eyes = pictures[eye1, eye2]
  where
    eye1 = color red $ translate 3 (-3) $ rectangleSolid 1 1
    eye2 =  color red $ translate 3 (-7) $ rectangleSolid 1 1

    
renderNode :: Node -> Picture
renderNode (Node nodeX nodeY _) = translate intNodeX intNodeY $ color constSnakeColor $ rectangleSolid floatBlockSize floatBlockSize
  where
    intNodeX = fromIntegral nodeX
    intNodeY = fromIntegral nodeY
    floatBlockSize = fromIntegral (constBlockSize - 1)

renderSeed :: Node -> Picture
renderSeed (Node nodeX nodeY _) = translate intNodeX intNodeY $ color constSeedColor $ rectangleSolid floatBlockSize floatBlockSize
  where
    intNodeX = fromIntegral nodeX
    intNodeY = fromIntegral nodeY
    floatBlockSize = fromIntegral constBlockSize

startingWorld :: Int -> World
startingWorld gen = World startingSnake (newRandomSeed gen) Menu (gen + 1)

gameOverWorld :: Int -> World
gameOverWorld gen = World startingSnake (newRandomSeed gen) GameOver (gen + 1)

newRandomSeed :: Int -> Node
newRandomSeed gen = Node x' y' NoDir
  where
    offsetDoBomSenso = 10                                               -- pra evitar que a semente fique muito perto da borda
    horizontalOffsetINT = round horizontalOffset - offsetDoBomSenso
    verticalOffsetINT = round verticalOffset - offsetDoBomSenso 
    (x', gen2) = randomR (-horizontalOffsetINT, horizontalOffsetINT) $ newGen gen
    (y', _) = randomR (-verticalOffsetINT, verticalOffsetINT) gen2

newGen :: Int -> StdGen
newGen = mkStdGen

renderWorld :: World -> Picture
renderWorld world 
  | gameState == Menu = renderMenu
  | gameState == GameOver = renderGameOver
  | gameState == Gaming = pictures [walls, renderSnake $ theSnake world, renderSeed $ seed world]
  | otherwise = pictures [walls, renderSnake $ theSnake world, renderSeed $ seed world]
  where 
    gameState = state world

renderMenu :: Picture
renderMenu = pictures [title, subtitlePlay, subtitleExit]
  where
    title = translate (-100) 200 renderTitle
    subtitlePlay = translate (-125) (-100) $ scale 0.15 0.15 $ Text "pressione 'ENTER' para jogar"
    subtitleExit = translate (-125) (-120) $ scale 0.15 0.15 $ Text "pressione 'Esc' para sair"

renderGameOver :: Picture
renderGameOver = pictures [title, subtitlePlay, subtitleExit]
  where
    title = translate (-100) 200 renderGameOverTitle
    subtitlePlay = translate (-125) (-100) $ scale 0.15 0.15 $ Text "pressione 'ENTER' para jogar"
    subtitleExit = translate (-125) (-120) $ scale 0.15 0.15 $ Text "pressione 'Esc' para sair"

startingSnake :: Snake
startingSnake = Snake { snakeHead = startingHead, snakeTail = startingTail startingHead, velocity = constVelocity }
  where
    startingHead = Node { x = 0, y = 0, direction = North}

startingTail :: Node -> [Node]
startingTail startingHead = startingTail' startingHead 5 []

startingTail' :: Node -> Int -> [Node] -> [Node]
startingTail' _ 0 initialTail = initialTail
startingTail' startingHead remaining [] = startingTail' startingHead (remaining - 1) [addNode startingHead]
startingTail' startingHead remaining ns = startingTail' startingHead (remaining - 1) $ ns ++ [newNode]
  where
    newNode = addNode lastNode
    lastNode = last ns

addNode :: Node -> Node
addNode (Node _ _ NoDir) = error "addNode must have a valid Direction"
addNode (Node nodeX nodeY North) = Node { x = nodeX, y = nodeY - constBlockSize, direction = North }
addNode (Node nodeX nodeY East) = Node { x = nodeX + constBlockSize, y = nodeY, direction = East }
addNode (Node nodeX nodeY South) = Node { x = nodeX, y = nodeY + constBlockSize, direction = South }
addNode (Node nodeX nodeY West) = Node { x = nodeX - constBlockSize, y = nodeY, direction = West }

stepWorld :: World ->  World
stepWorld (World snk s Menu gen) = World snk s Menu gen
stepWorld (World snk s GameOver gen) = World snk s GameOver gen
stepWorld world 
  | hasEated movedSnake currentSeed = World newSnake (newRandomSeed (gen + 1)) Gaming gen
  | snakeCollided movedSnake || wallCollided snakeHead' = gameOverWorld gen               -- Como não tem um "Game over" implementado ele simplesmente reseta
  | otherwise = World movedSnake currentSeed state' (gen + 1)
  where
    state' = state world
    movedSnake = moveSnake (theSnake world) 
    currentSeed = seed world
    newSnake = insertNewNode movedSnake
    gen = randomnessSeed world
    snakeHead' = snakeHead movedSnake 

insertNewNode :: Snake -> Snake
insertNewNode snake = Snake { snakeHead = snakeHead snake, snakeTail = incrementTail $ snakeTail snake, velocity = velocity snake }

incrementTail :: [Node] -> [Node]
incrementTail [] = []
incrementTail [n] = [ n, addNode n ]
incrementTail (n:ns) = n:incrementTail ns

hasEated :: Snake -> Node -> Bool
hasEated snake s1 = nodeCollided h s1
  where 
    h = snakeHead snake

nodeCollided :: Node -> Node -> Bool
nodeCollided (Node x1 y1 _) (Node x2 y2 _) = pythagoreanDistanceSquared < constBlockSize ^ 2
  where
    square = 2
    pythagoreanDistanceSquared = (x2 - x1) ^ square + (y2 - y1) ^ square

-- Given a snake model and a direction move snake one or more blocks in the direction
moveSnake :: Snake ->  Snake
moveSnake (Snake h t vel)  = Snake newHead newTail vel
  where 
    currentDir = direction h
    newHead =  moveNode h vel currentDir
    newTail = moveTail t h

-- moves a Node to a given direction in a given velocity (velocity = blocks)
moveNode :: Node -> Int -> Direction -> Node
moveNode node _ NoDir = node
moveNode (Node nodeX nodeY _) vel North = Node { x = nodeX, y = nodeY + vel, direction = North}
moveNode (Node nodeX nodeY _) vel East  = Node { x = nodeX + vel, y = nodeY, direction = East}
moveNode (Node nodeX nodeY _) vel South = Node { x = nodeX, y = nodeY - vel, direction = South}
moveNode (Node nodeX nodeY _) vel West  = Node { x = nodeX - vel, y = nodeY, direction = West }

oppositeDirection :: Direction -> Direction
oppositeDirection North = South
oppositeDirection East = West
oppositeDirection South = North
oppositeDirection West = East
oppositeDirection NoDir = NoDir

keepOrChangeDirection :: Direction -> Direction -> Direction
keepOrChangeDirection current NoDir = current
keepOrChangeDirection current new
  | current == oppositeDirection new = current
  | otherwise = new

moveTail :: [Node] -> Node -> [Node]
moveTail [] _ = []
moveTail (x':xs) targetNode = targetNode:(moveTail xs x')

wallCollided :: Node -> Bool
wallCollided snakeHead'  = bottomCollision || topCollision || rightCollision || leftCollision
  where
    xHead = fromIntegral $ x snakeHead'
    yHead = fromIntegral $ y snakeHead'
    blockColisionRange = fromIntegral $ constBlockSize `div` 2
    topLimit = verticalOffset - blockColisionRange
    rightLimit = horizontalOffset - blockColisionRange
    bottomLimit = (-verticalOffset) + blockColisionRange
    leftLimit = (-horizontalOffset) + blockColisionRange
    topCollision = yHead > topLimit
    rightCollision = xHead > rightLimit
    bottomCollision = yHead < bottomLimit
    leftCollision = xHead < leftLimit

snakeCollided :: Snake -> Bool
snakeCollided (Snake _ [] _) = False
snakeCollided (Snake h ts _) = headIsOnTail h $ tail $ tail ts

headIsOnTail :: Node -> [Node] -> Bool
headIsOnTail _ [] = False
headIsOnTail h (t:ts)  
  | nodeCollided h t = True
  | otherwise = headIsOnTail h ts

updatePlayWorld :: Float -> World -> World
updatePlayWorld _ = stepWorld 

handleKeysGaming :: SpecialKey -> World -> World
handleKeysGaming key (World snake seed' _ gen) 
  | key == KeyUp =  buildWorld snake (keepOrChangeDirection currentDirection North) seed' Gaming (gen - 5)
  | key == KeyRight = buildWorld snake (keepOrChangeDirection currentDirection East) seed' Gaming (gen - 5)
  | key == KeyDown = buildWorld snake (keepOrChangeDirection currentDirection South) seed' Gaming (gen - 5)
  | key == KeyLeft = buildWorld snake (keepOrChangeDirection currentDirection West) seed' Gaming (gen -5)
  | otherwise = buildWorld snake (keepOrChangeDirection currentDirection NoDir) seed' Gaming (gen - 5)
  where
    currentDirection = direction (snakeHead snake)

handleKeysMenu :: SpecialKey -> World -> World
handleKeysMenu KeyEnter (World snake seed' _ gen) = World snake seed' Gaming gen
handleKeysMenu _ world = world

handleKeysWorld :: Event -> World -> World
handleKeysWorld (EventKey (SpecialKey key) _ _ _) (World snake seed' Gaming gen) = handleKeysGaming key (World snake seed' Gaming gen)
handleKeysWorld (EventKey (SpecialKey key) _ _ _) (World snake seed' Menu gen) = handleKeysMenu key (World snake seed' Menu gen)
handleKeysWorld (EventKey (SpecialKey key) _ _ _) (World snake seed' GameOver gen) = handleKeysMenu key (World snake seed' GameOver gen)
handleKeysWorld _ world = world