{-# OPTIONS_GHC -Wall #-}

import CodeWorld
import Numeric (readHex)

-- Exercise 1

frame :: Picture
frame = rectangle 2.5 7.5

transLight :: Double -> Picture
transLight dy = translated 0 dy (solidCircle 1)

topLight :: Color -> Picture
topLight c = colored c (transLight 2.5)

midLight :: Color -> Picture
midLight c = colored c (transLight 0)

botLight :: Color -> Picture
botLight c = colored c (transLight (-2.5))

lightPhase :: Color -> Color -> Color -> Picture
lightPhase tc mc bc = topLight tc & midLight mc & botLight bc & frame

calcTick :: Double -> Int
calcTick t = mod (round t) 8

    {-
       traffic light states
       0 : green
       1 : yellow
       2 : red
       -}
trafficController :: Double -> Picture
trafficController t
  | calcTick t <  3 = lightPhase black black  green
  | calcTick t == 3 = lightPhase black yellow black
  | calcTick t <  7 = lightPhase red   black  black
  | calcTick t == 7 = lightPhase red   yellow black
  | otherwise       = blank

exercise1 :: IO ()
exercise1 = animationOf trafficController

-- Exercise 2

branch :: Bool -> Integer -> Double -> Picture
branch True  b r = rotated (  pi/10) (tree b r)
branch False b r = rotated (- pi/10) (tree b r)

tree :: Integer -> Double -> Picture
tree 0 r = colored yellow (solidCircle (0.04*r))
tree n r = translated 0 1 (branch True (n-1) r & branch False (n-1) r) 
      & polyline [(0,0),(0,1)]

treeController :: Double -> Picture
treeController t
  | t < 10    = tree 8 t
  | otherwise = tree 8 10

exercise2 :: IO ()
exercise2 = animationOf treeController

-- Exercise 3

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

drawRow :: Int -> Picture
drawRow i 
  | i < -4    = blank
  | otherwise = translated (fromIntegral i) 0 (drawCol i 4) & drawRow (i-1)

drawCol :: Int -> Int -> Picture
drawCol i j
  | j < -4    = blank
  | otherwise = translated 0 (fromIntegral j) (drawTile (maze i j)) & drawCol i (j-1)

pictureOfMaze :: Picture
pictureOfMaze = drawRow 4

exercise3 :: IO ()
exercise3 = drawingOf pictureOfMaze

-- Main

main :: IO ()
-- main = exercise1
-- main = exercise2
main = exercise3

