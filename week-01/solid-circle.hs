{-# OPTIONS_GHC -Wall #-}
import CodeWorld

ourPicture :: Picture
ourPicture = solidCircle 1

main :: IO ()
main = drawingOf ourPicture
