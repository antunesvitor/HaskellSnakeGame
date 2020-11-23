module Letters where

import Graphics.Gloss

data FontConfiguration = FontConfiguration {
    backgroundColor :: Color,
    color :: Color,
    size :: Int
}