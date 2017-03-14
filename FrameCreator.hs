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
import qualified Diagrams.TwoD.Size as Dia
import Diagrams.Prelude hiding (render)
import Diagrams.Backend.Rasterific
import Codec.Picture.Png
import qualified Codec.Picture.Types as JP
import Vision.Image.JuicyPixels
import Vision.Image.Filter (dilate, blur)
import Vision.Image.Conversion
import Vision.Image.Grey
import Vision.Image.Mutable
import Vision.Image.Type
import qualified Vision.Image.Class as V
import Vision.Primitive.Shape (ix2)
import Vision.Image.Transform
import Control.Monad.ST.Safe

parsePatternData ::
     (ByteString, Int, ForeGroundTemplate)
  -> Maybe (PatternData)
parsePatternData (bsData, c, t) =
  PatternData <$> pd <*> pure t
    <*> (getDefaultRadius c <$> pd)
    <*> pure c
  where
    pd = case loadImageEmbBS bsData of
          (Left _) -> Nothing
          (Right dimg) -> Just $ image dimg

parseBackgroundImageData ::
     ByteString
  -> Maybe (BackgroundImage)
parseBackgroundImageData bsData = BackgroundImage <$> pd
  where
    pd = case loadImageEmbBS bsData of
          (Left _) -> Nothing
          (Right dimg) -> Just $ image dimg

encodeToPng :: Diagram Rasterific -> Int -> ByteString
encodeToPng dia width = Data.ByteString.Lazy.toStrict $
  encodePng $ render dia width

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
  -> MaskParams
  -> (ByteString, ByteString, ByteString, JP.Image JP.Pixel8)
getMask dia width (MaskParams dilValue blurVal) =
  (enc dilatedJpData
  , enc ffJpData
  , enc subtractedJpData
  , subtractedJpData)

  where
    enc i = Data.ByteString.Lazy.toStrict $ encodePng i

    jpData :: JP.Image JP.PixelRGB8
    jpData = JP.pixelMap invertTransparent (render dia width)

    -- An alpha value of zero represents full transparency,
    -- and a value of (2^bitdepth)-1 represents a 
    -- fully opaque pixel

    -- Invert - Opaque -> White
    invertTransparent (JP.PixelRGBA8 0 0 0 a) = JP.PixelRGB8 a a a
    -- Transparent -> Black
    invertTransparent (JP.PixelRGBA8 0 0 0 0) = JP.PixelRGB8 0 0 0
    invertTransparent p = JP.dropTransparency p

    -- :: JP.Image PixelRGB8 -> RGB
    jpGrey = JP.extractLumaPlane jpData

    greyImg :: Grey
    greyImg = toFridayGrey jpGrey

    blurredImg :: Grey
    blurredImg = blur blurVal greyImg

    dilatedImage :: Grey
    dilatedImage = dilate dilValue blurredImg

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

render ::
     Diagram Rasterific
  -> Int
  -> JP.Image JP.PixelRGBA8
render dia width = renderDia Rasterific
          (RasterificOptions (mkWidth w))
          (dia)
  where
    w :: Double
    w = fromIntegral width


renderSquareImage :: Diagram Rasterific -> Int -> JP.Image JP.PixelRGBA8
renderSquareImage diaImg width = squareImage

  where
    dw = (Dia.width diaImg)
    dh = (Dia.height diaImg)

    w :: Double
    w = fromIntegral width
    sspec = if dw < dh then mkWidth w else mkHeight w
    img = renderDia Rasterific (RasterificOptions sspec) diaImg

    -- squareImage :: JP.Pixel a => JP.Image a
    squareImage = JP.generateImage
      (\x y -> JP.pixelAt img x y) edge edge
    edge = min (JP.imageWidth img) (JP.imageHeight img)


createFrame ::
     BackgroundImage
  -> ForeGround
  -> Mask
  -> Int
  -> ByteString

createFrame img fg mask width =
  Data.ByteString.Lazy.toStrict $ encodePng maskedImgJpData
  where
    -- Create the raster of all three of same size
    -- Make the mask portion invisible for image
    -- Add the frame on top of this

    imgJP = renderSquareImage (origBackgroundImage img) width
    fgJP = render (foreGroundDia fg) width

    maskJP = JP.pixelMap modMask $ maskData mask

    modMask :: JP.Pixel8 -> JP.PixelRGBA8
    -- Black portion, outside
    modMask 0 = JP.PixelRGBA8 0 0 0 0
    -- White portion, inside
    modMask _ = JP.PixelRGBA8 1 1 1 1

    maskedImgJpData =
      JP.zipPixelComponent3 doMasking
        imgJP fgJP maskJP

    doMasking i f m =
      if m > 0 then i else f

