module Letters where

import Graphics.Gloss

data FontConfiguration = FontConfiguration {
    backgroundColor :: Color,
    fontColor :: Color,
    size :: Int
}

renderGameOverTitle :: Picture
renderGameOverTitle = pictures [g,a,m,e1,o,v,e2,r]
  where
    g = translate 0 0 $ renderCharacter getGconfig
    a = translate 37 0 $ renderCharacter getAconfig
    m = translate 74 0 $ renderCharacter getMconfig
    e1 = translate 111 0 $ renderCharacter getEconfig
    o = translate 0 (-49) $ renderCharacter getOConfig
    v = translate 37 (-49) $ renderCharacter getVconfig
    e2 = translate 74 (-49) $ renderCharacter getEconfig
    r = translate 111 (-49) $ renderCharacter getRConfig

renderTitle :: Picture
renderTitle = pictures [s,n,a,k,e,g,a2,m,e2]
  where
    s = renderCharacter getSconfig
    n = translate 37 0 $ renderCharacter getNconfig
    a = translate 74 0 $ renderCharacter getAconfig
    k = translate 111 0 $ renderCharacter getKconfig
    e = translate 148 0 $ renderCharacter getEconfig
    g = translate 18 (-49) $ renderCharacter getGconfig
    a2 = translate 55 (-49) $ renderCharacter getAconfig
    m = translate 92 (-49) $ renderCharacter getMconfig
    e2 = translate 129 (-49) $ renderCharacter getEconfig

letterBackground :: Picture
letterBackground = color black $ rectangleSolid' 36 48 

renderCharacter :: [Picture] -> Picture
renderCharacter charCfg = alinharCentro $ pictures (letterBackground:charCfg)

-- todas as letras a principio terão 30x40 pixel
getSconfig :: [Picture]
getSconfig = [linhaSup, linhaInf]
-- renderS = pictures [retSuperior, retMedioSuperior, retMedio, retMedioInferior, retInferior]
  where
    whiteTrace = whiten $ rectangleSolid' 24 1
    linhaSup = translate 12 (-19) whiteTrace
    linhaInf = translate 0 (-30) whiteTrace

getAconfig :: [Picture]
getAconfig = [linha1, linha2]
  where 
    linha1 = translate 18 (-12) $ whiten $ rectangleSolid' 1 6
    linha2 = translate 18 (-24) $ whiten $ rectangleSolid' 1 24

getNconfig :: [Picture]
getNconfig = [linha1, linha2]
  where
    whiteTrace = rectangleSolid' 1 24
    linha1 = translate 24 0 $ whiten whiteTrace
    linha2 = translate 12 (-24) $ whiten whiteTrace

getKconfig :: [Picture]
getKconfig = [linha1, linha2, linha3]
  where
    whiteThinTrace = whiten $ rectangleSolid' 1 16
    whiteThickTrace = whiten $ rectangleSolid' 16 1
    linha1 = translate 12 0 whiteThinTrace 
    linha2 = translate 12 (-32) whiteThinTrace
    linha3 = translate 20 (-24) whiteThickTrace

getEconfig :: [Picture]
getEconfig = [linha1, linha2]
  where
    whiteTrace = color white $ rectangleSolid' 24 1
    linha1 = translate 12 (-16) whiteTrace
    linha2 = translate 12 (-32) whiteTrace

getGconfig :: [Picture]
getGconfig =  [linha1, linha2, linha3, linha4]
  where
    linha1 = translate 12 (-12) $ whiten $ rectangleSolid' 24 1
    linha2 = translate 12 (-12) $ whiten $ rectangleSolid' 1 24
    linha3 = translate 12 (-36) $ whiten $ rectangleSolid' 12 1
    linha4 = translate 24 (-25) $ whiten $ rectangleSolid' 1 12

getMconfig :: [Picture]
getMconfig = [linha1,linha2, linha3]
  where
    whiteTrace = whiten $ rectangleSolid' 1 32
    linha1 = translate 12 (-16) whiteTrace
    linha2 = translate 24 (-16) whiteTrace    
    linha3 = translate 18 0 $ whiten $ rectangleSolid' 1 6

getOConfig :: [Picture]
getOConfig = [linha1]
  where
    linha1 = translate 18 (-12) $ whiten $ rectangleSolid' 1 24
  
getVconfig :: [Picture]
getVconfig = [linha1, trianguloBorda]
  where
    linha1 = translate 18 0 $ whiten $ rectangleSolid' 1 36
    trianguloBorda = whiten $ Polygon [(30,-48), (36, -42), (36, -48)]

getRConfig :: [Picture]
getRConfig = [linha1, linha2, linha3]
  where
    linha1 = translate 12 (-35) $ whiten $ rectangleSolid' 1 13
    linha2 = translate 24 (-26) $ whiten $ rectangleSolid' 12 1
    linha3 = translate 12 (-12) $ whiten $ rectangleSolid' 13 1

whiten :: (Picture -> Picture)
whiten = color white

alinharCentro :: (Picture -> Picture)
alinharCentro = translate 18 (-24)

orientationPoint :: Picture
orientationPoint = rectangleSolid 1 1

rectangleSolid' :: Float -> Float -> Picture
rectangleSolid' x y = translate (x/2) (-y/2) $ rectangleSolid x y