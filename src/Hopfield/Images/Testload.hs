module Main where

import ConvertImage

main = do
  l <- loadPicture "../images/3x3.bmp"
  print l
