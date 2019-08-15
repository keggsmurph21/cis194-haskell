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

ourPicture :: Picture
ourPicture = trafficLight False

main :: IO ()
main = drawingOf ourPicture
