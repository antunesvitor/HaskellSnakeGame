module Main where

import SnakeGameWorld
import Graphics.Gloss

main :: IO ()

background :: Color
background = makeColor 200 200 200 0

renderTitle :: Picture
renderTitle = translate (-100) 100 $ pictures [s,n,a,k,e,g,a2,m,e2]
  where
    s = renderS
    n = translate 37 0 renderN
    a = translate 74 0 renderA
    k = translate 111 0 renderK
    e = translate 148 0 renderE
    g = translate 18 (-49) renderG
    a2 = translate 55 (-49) renderA
    m = translate 92 (-49) renderM
    e2 = translate 129 (-49) renderE

letterBackground :: Picture
letterBackground = color black $ rectangleSolid' 36 48 

-- todas as letras a principio terão 30x40 pixel
renderS :: Picture
renderS = alinharCentro $ pictures [letterBackground, linhaSup, linhaInf]
-- renderS = pictures [retSuperior, retMedioSuperior, retMedio, retMedioInferior, retInferior]
  where
    whiteTrace = color white $ rectangleSolid' 24 1
    linhaSup = translate 12 (-19) whiteTrace
    linhaInf = translate 0 (-30) whiteTrace

renderA :: Picture
renderA = alinharCentro $ pictures [letterBackground, linha1, linha2]
  where 
    linha1 = translate 18 (-12) $ color white $ rectangleSolid' 1 6
    linha2 = translate 18 (-24) $ color white $ rectangleSolid' 1 24

renderN :: Picture
renderN = alinharCentro $ pictures [letterBackground, linha1, linha2]
  where
    whiteTrace = rectangleSolid' 1 24
    linha1 = translate 24 0 $ color white whiteTrace
    linha2 = translate 12 (-24) $ color white whiteTrace

renderK :: Picture 
renderK = alinharCentro  $ pictures [letterBackground, linha1, linha2, linha3]
  where
    whiteThinTrace = color white $ rectangleSolid' 1 16
    whiteThickTrace = color white $ rectangleSolid' 16 1
    linha1 = translate 12 0 whiteThinTrace 
    linha2 = translate 12 (-32) whiteThinTrace
    linha3 = translate 20 (-24) whiteThickTrace

renderE :: Picture
renderE = alinharCentro $ pictures [letterBackground, linha1, linha2]
  where
    whiteTrace = color white $ rectangleSolid' 24 1
    linha1 = translate 12 (-16) whiteTrace
    linha2 = translate 12 (-32) whiteTrace

renderG :: Picture
renderG = alinharCentro $ pictures [letterBackground, linha1, linha2, linha3, linha4]
  where
    linha1 = translate 12 (-12) $ whiten $ rectangleSolid' 24 1
    linha2 = translate 12 (-12) $ whiten $ rectangleSolid' 1 24
    linha3 = translate 12 (-36) $ whiten $ rectangleSolid' 12 1
    linha4 = translate 24 (-25) $ whiten $ rectangleSolid' 1 12

renderM :: Picture
renderM = alinharCentro $ pictures [letterBackground, linha1,linha2, linha3]
  where
    whiteTrace = whiten $ rectangleSolid' 1 32
    linha1 = translate 12 (-16) whiteTrace
    linha2 = translate 24 (-16) whiteTrace    
    linha3 = translate 18 0 $ whiten $ rectangleSolid' 1 6

whiten :: (Picture -> Picture)
whiten = color white

alinharCentro :: (Picture -> Picture)
alinharCentro = translate 18 (-24)

orientationPoint :: Picture
orientationPoint = rectangleSolid 1 1

rectangleSolid' :: Float -> Float -> Picture
rectangleSolid' x y = translate (x/2) (-y/2) $ rectangleSolid x y

main = do
  -- print horizontalOffset
  -- play window background 30 startingWorld renderWorld handleKeysWorld updatePlayWorld
    display window white renderTitle
  where
    window = (InWindow "Snake Game" (320, 568) (10, 10))