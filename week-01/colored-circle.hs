{-# OPTIONS_GHC -Wall #-}
import CodeWorld

ourPicture :: Picture
ourPicture = colored green (solidCircle 1)

main :: IO ()
main = drawingOf ourPicture
