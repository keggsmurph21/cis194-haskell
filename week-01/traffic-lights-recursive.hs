{-# OPTIONS_GHC -Wall #-}
import CodeWorld

bottomCircle :: Color -> Picture
bottomCircle c = colored c (translated 0 (-1.5) (solidCircle 1))

topCircle :: Color -> Picture
topCircle c    = colored c (translated 0   1.5  (solidCircle 1))

frame :: Picture
frame = rectangle 2.5 5.5

trafficLight :: Bool -> Picture
trafficLight True  = bottomCircle green & topCircle black & frame
trafficLight False = bottomCircle black & topCircle red   & frame

spread :: Picture -> Double -> Integer -> Picture
spread _ _  0 = blank
spread pic dx n = pic & translated dx 0 (spread pic dx (n-1))

ourPicture :: Picture
ourPicture = spread (trafficLight True) 3 5

main :: IO ()
main = drawingOf ourPicture
