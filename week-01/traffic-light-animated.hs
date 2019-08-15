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

trafficController :: Double -> Picture
trafficController t
    | mod (round (t/3)) 2 == 0 = trafficLight True
    -- | round (t/3) `mod` 2 == 0 = trafficLight True
    | otherwise                = trafficLight False

main :: IO ()
main = animationOf trafficController 
