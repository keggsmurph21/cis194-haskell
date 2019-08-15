{-# OPTIONS_GHC -Wall #-}
import CodeWorld

ourPicture :: Picture
ourPicture = colored green (solidCircle 1) & solidCircle 2

main :: IO ()
main = drawingOf ourPicture
