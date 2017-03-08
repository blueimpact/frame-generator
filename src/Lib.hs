{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE TypeFamilies              #-}
module Lib where

import Diagrams.TwoD.Image
import Diagrams.TwoD.Size
import Diagrams.Prelude
import Diagrams.Backend.Rasterific

myCircle :: Diagram B
myCircle = circle 1000

main = do
  putStrLn "Hello"
  let sizeSpec = mkSizeSpec2D (Just 600) (Nothing)

  (Right img') <- loadImageEmb "0003.png"
  let 
      origImg :: Diagram B
      origImg = image img'

      scaledImg = scale 0.5 origImg

      num = 32
      radius = getDefaults (width scaledImg, height scaledImg) num
  
      rotOffset = 0
      finalImg = getImageAndPosition scaledImg num radius rotOffset




  renderRasterific "output.png" sizeSpec finalImg

getImageAndPosition :: Diagram B -> Int -> Double -> Double -> Diagram B
getImageAndPosition img num radius rotOffset = mconcat $ map snd transList
  where
    imgL1 :: [(Int, Diagram B)]
    imgL1 = zip (0:[1..]) $ take num (repeat img)

    rotatedList :: [(Int, Diagram B)]
    rotatedList = map (\(n,i) -> (n,rotateBy ((fromIntegral n)/(fromIntegral num)) i)) imgL1

    rotOffsetApplied :: [(Int, Diagram B)]
    rotOffsetApplied = map (\(n,i) -> (n,rotate (rotOffset @@ deg) i)) rotatedList

    transList = 
      map (\(n,i) -> (n, translateX (x n) (translateY (y n) i))) rotOffsetApplied
        where 
          x n = g sin n
          y n = g cos n
          g f n = radius*f ((((-2)*(fromIntegral n))/(fromIntegral num))*pi)


-- Default parameters determination
--
-- alpha : angle between two patterns = (360/num)
--
-- Radius = (height/2) + (width/2)*(cot (alpha/2))

getDefaults :: (Double, Double) -> Int -> Double
getDefaults (w,h) num' = (h/2) + (w/2)/(tan (alpha/2))
  where
    num = fromIntegral num'
    alpha = (2*pi)/num
