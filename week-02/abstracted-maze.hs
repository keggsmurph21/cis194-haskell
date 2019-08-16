{-# OPTIONS_GHC -Wall #-}

import CodeWorld

colorFactory :: Double -> Double -> Double -> Color
colorFactory r g b = RGB (r/255) (g/255) (b/255)

groundColor, storageColor, boxBgColor, boxFgColor, wallColor :: Color
groundColor = colorFactory 222 214 174
storageColor = colorFactory 214 149 133
boxBgColor = colorFactory 141 44 0
boxFgColor = colorFactory 186 101 12
wallColor = colorFactory 161 149 85

tile, wall, ground, storage, box :: Picture
tile = solidRectangle 1 1
wall = colored wallColor tile
ground = colored groundColor tile
storage = colored storageColor (solidCircle 0.2) & ground
box = colored boxFgColor (solidRectangle 0.7 0.7) & colored boxBgColor tile

drawTileAt :: Int -> Int -> Picture
drawTileAt i j = translated (fromIntegral i) (fromIntegral j) (drawTile (maze i j))

drawTile :: Int -> Picture
drawTile i
  | i == 1    = wall
  | i == 2    = ground
  | i == 3    = storage
  | i == 4    = box
  | otherwise = blank

maze :: Int -> Int -> Int
maze x y
  | abs x > 4  || abs y > 4  = 0
  | abs x == 4 || abs y == 4 = 1
  | x ==  2 && y <= 0        = 1
  | x ==  3 && y <= 0        = 3
  | x >= -2 && y == 0        = 4
  | otherwise                = 2

iter :: (Int -> Picture) -> Picture
iter func = 
    let _iter :: (Int -> Picture) -> Int -> Picture
        _iter _ (-5) = blank
        _iter _func n = _func n & _iter _func (n-1)
    in _iter func 4

    {- alternatively...
iter :: (Int -> Picture) -> Picture
iter func = _iter func 4
    where
        _iter :: (Int -> Picture) -> Int -> Picture
        _iter _ (-5) = blank
        _iter _func n = _func n & _iter _func (n-1)

    -- or ...
iter :: (Int -> Picture) -> Picture
iter func = go 4
    where
        go (-5) = blank
        go   n  = func n & go (n-1)
-}

pictureOfMaze :: Picture
pictureOfMaze = iter (\i -> iter (\j -> drawTileAt i j))
-- pictureOfMaze = iter drawRows

-- drawRows :: Int -> Picture
-- drawRows i = iter (drawTileAt i)

-- drawCols :: Int -> Int -> Picture
-- drawCols i j = drawTileAt i j

main :: IO ()
main = drawingOf pictureOfMaze

