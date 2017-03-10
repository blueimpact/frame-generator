{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE TypeFamilies              #-}

-- Algo for doing image creation/manipulation
module FrameCreator where

import AppData

import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy

import Diagrams.TwoD.Image
import Diagrams.TwoD.Size
import Diagrams.Prelude
import Diagrams.Backend.Rasterific
import Codec.Picture.Png

parseImageData ::
     (ByteString, Int)
  -> Maybe (PatternData)
parseImageData (bsData, c) =
  PatternData <$> pd <*> pure Horizontal
    <*> (getDefaultRadius c <$> pd)
    <*> pure c
  where
    pd = case loadImageEmbBS bsData of
          (Left _) -> Nothing
          (Right dimg) -> Just $ image dimg

encodeToPng :: Diagram Rasterific -> Int -> ByteString
encodeToPng dia width = Data.ByteString.Lazy.toStrict $
  encodePng $ renderDia Rasterific
          (RasterificOptions (mkWidth w))
          (dia)
  where
    w :: Double
    w = fromIntegral width

getDefaultRadius :: Int -> Diagram B -> Double
getDefaultRadius num' img = (h/2) + (w/2)/(tan (alpha/2))
  where
    w = width img
    h = height img
    num = fromIntegral num'
    alpha = (2*pi)/num

getForeGround ::
     ForeGroundParams
  -> Diagram Rasterific
  -> Diagram Rasterific
getForeGround
  (ForeGroundParams num radius rotOffset scaling
    radiusOffset tmplt)
  img
  = mconcat $ map snd finalList
  where
    finalList = transList

    scaledImg = scale scaling img

    imgL1 :: [(Int, Diagram B)]
    imgL1 = zip (0:[1..]) $ take num (repeat scaledImg)

    rotatedList :: [(Int, Diagram B)]
    rotatedList = map (\(n,i) ->
      (n,rotateBy ((fromIntegral n)/(fromIntegral num)) i)) imgL1

    rotOffsetApplied :: [(Int, Diagram B)]
    rotOffsetApplied = map (\(n,i) -> (n,rotate (rotOffset @@ deg) i)) rotatedList

    -- Apply transformations
    transList =
      map (\(n,i) -> (n, translateX (x n) (translateY (y n) i))) rotOffsetApplied
        where
          x n = g sin n
          y n = g cos n
          g f n = radius*f ((((-2)*(fromIntegral n))/(fromIntegral num))*pi)
