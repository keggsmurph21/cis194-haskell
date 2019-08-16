{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}

import CodeWorld

data Direction = R | U | L | D
data Coord = C Integer Integer
data Tile = Wall | Ground | Storage | Box | Blank

colorFactory :: Double -> Double -> Double -> Color
colorFactory r g b = RGB (r/255) (g/255) (b/255)

groundColor, storageColor, boxBgColor, boxFgColor, wallColor :: Color
groundColor = colorFactory 222 214 174
storageColor = colorFactory 214 149 133
boxBgColor = colorFactory 141 44 0
boxFgColor = colorFactory 186 101 12
wallColor = colorFactory 161 149 85

wall, ground, storage, box :: Picture
wall = colored wallColor (solidRectangle 1 1)
ground = colored groundColor (solidRectangle 1 1)
storage = colored storageColor (solidCircle 0.2) & ground
box = colored boxFgColor (solidRectangle 0.7 0.7) & colored boxBgColor (solidRectangle 1 1)

drawTileAt :: Int -> Int -> Picture
drawTileAt i j = translated (fromIntegral i) (fromIntegral j) (drawTile (maze i j))

drawTile :: Tile -> Picture
drawTile Wall    = wall
drawTile Ground  = ground
drawTile Storage = storage
drawTile Box     = box
drawTile Blank   = blank

maze :: Int -> Int -> Tile
maze x y
  | abs x > 4  || abs y > 4  = Blank
  | abs x == 4 || abs y == 4 = Wall
  | x ==  2 && y <= 0        = Wall
  | x ==  3 && y <= 0        = Storage
  | x >= -2 && y == 0        = Box
  | otherwise                = Ground

iter :: (Int -> Picture) -> Picture
iter func = go 4
    where
        go (-5) = blank
        go   n  = func n & go (n-1)

pictureOfMaze :: Picture
pictureOfMaze = iter (\i -> iter (\j -> drawTileAt i j))

main :: IO ()
main = interactionOf initCoord handleTime handleEvent drawState

initCoord :: Coord
initCoord = C 0 0

atCoord :: Coord -> Picture -> Picture
atCoord (C x y) pic = translated (fromIntegral x) (fromIntegral y) pic

adjCoord :: Direction -> Coord -> Coord
adjCoord R (C x y) = C (x+1)  y
adjCoord U (C x y) = C  x    (y+1)
adjCoord L (C x y) = C (x-1)  y
adjCoord D (C x y) = C  x    (y-1)

someCoord :: Coord
someCoord = adjCoord U (adjCoord U (adjCoord L initCoord))

handleTime :: Double -> Coord -> Coord
handleTime _ c = c

handleEvent :: Event -> Coord -> Coord
handleEvent (KeyPress key) c 
  | key == "Right" = adjCoord R c
  | key == "Up"    = adjCoord U c
  | key == "Left"  = adjCoord L c
  | key == "Down"  = adjCoord D c
handleEvent _ c = c
  
drawState :: Coord -> Picture
drawState c = atCoord c pictureOfMaze
