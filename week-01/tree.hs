{-# OPTIONS_GHC -Wall #-}
import CodeWorld

tree :: Integer -> Picture
tree 0 = blank
tree n = polyline [(0,0),(0,1)] & translated 0 1 (
    rotated (pi/10) (tree (n-1)) & rotated (- pi/10) (tree (n-1)))

main :: IO ()
main = drawingOf (tree 10)
