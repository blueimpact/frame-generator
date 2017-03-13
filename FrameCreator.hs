{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE AllowAmbiguousTypes       #-}

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
import qualified Codec.Picture.Types as JP
import Vision.Image.JuicyPixels
import Vision.Image.Filter (dilate)
import Vision.Image.Conversion
import Vision.Image.Grey
import Vision.Image.Mutable
import Vision.Image.Type
import qualified Vision.Image.Class as V
import Vision.Primitive.Shape (ix2)
import Vision.Image.Transform
import Control.Monad.ST.Safe

parseImageData ::
     (ByteString, Int, ForeGroundTemplate)
  -> Maybe (PatternData)
parseImageData (bsData, c, t) =
  PatternData <$> pd <*> pure t
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

getMask ::
     Diagram Rasterific
  -> Int
  -> Int
  -> (ByteString, ByteString, ByteString)
getMask dia width dilValue =
  (enc dilatedJpData, enc ffJpData, enc subtractedJpData)

  where
    enc i = Data.ByteString.Lazy.toStrict $ encodePng i

    jpData :: JP.Image JP.PixelRGB8
    jpData = JP.pixelMap modTransparent $ renderDia Rasterific
              (RasterificOptions (mkWidth w))
              (dia)

    -- Turn non-transparent pixel to black
    modTransparent (JP.PixelRGBA8 0 0 0 a) = JP.PixelRGB8 a a a
    modTransparent p = JP.dropTransparency p

    -- :: JP.Image PixelRGB8 -> RGB
    jpGrey = JP.extractLumaPlane jpData

    greyImg :: Grey
    greyImg = toFridayGrey jpGrey

    dilatedImage :: Manifest GreyPixel
    dilatedImage = dilate dilValue greyImg

    dilatedJpData = toJuicyGrey dilatedImage

    ffImg :: Grey
    ffImg = create (doFloodFill dilatedImage (GreyPixel 255))
      where
        doFloodFill inpImg pxl = do
          dilImg <- thaw inpImg :: ST s (MutableManifest GreyPixel s)
          floodFill (ix2 (ceiling (w/2)) (ceiling (w/2)))
            pxl dilImg
          return dilImg

    ffJpData = toJuicyGrey ffImg

    subtractedJpData = JP.zipPixelComponent3 subtract
                        ffJpData dilatedJpData dilatedJpData
      where subtract v1 v2 _ = max 0 (v1 - v2)

    w :: Double
    w = fromIntegral width


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
    initRot =
      case tmplt of
        Horizontal -> 0
        Vertical -> -90
        Diagnol -> -45

    finalList = transList

    scaledImg = scale scaling img

    imgL1 :: [(Int, Diagram B)]
    imgL1 = zip (0:[1..]) $ take num (repeat scaledImg)

    rotatedList :: [(Int, Diagram B)]
    rotatedList = map (\(n,i) ->
      (n,rotateBy ((fromIntegral n)/(fromIntegral num)) i)) imgL1

    rotOffsetApplied :: [(Int, Diagram B)]
    rotOffsetApplied = map (\(n,i) ->
      (n,rotate ((initRot + rotOffset) @@ deg) i)) rotatedList

    -- Apply transformations
    transList =
      map (\(n,i) -> (n, translateX (x n) (translateY (y n) i))) rotOffsetApplied
        where
          x n = g sin n
          y n = g cos n
          g f n = radius*f ((((-2)*(fromIntegral n))/(fromIntegral num))*pi)
