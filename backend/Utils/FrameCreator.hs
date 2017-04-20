{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE AllowAmbiguousTypes       #-}

-- Algo for doing image creation/manipulation
module Utils.FrameCreator where

import Common

import Prelude

import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as BSL

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

import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NE

encodeToPng :: Diagram Rasterific -> Int -> ByteString
encodeToPng dia width = BSL.toStrict $
  encodePng $ render dia width

encodeToPngLazy :: Diagram Rasterific -> Int -> BSL.ByteString
encodeToPngLazy dia width =
  encodePng $ render dia width


-- Depend on the template
getDefaultRadius :: Int -> PatternShape -> Diagram B -> Double
getDefaultRadius num' t img =
  case t of
    Horizontal -> horiz
    Vertical -> vert
    Diagnol -> diag
  where
    horiz = (h/2) + (w/2)/(tan (alpha/2))
    vert = (w/2) + (h/2)/(tan (alpha/2))
    diag = (w/root2) *(1/(sin (alpha/2))) -- Assuming w == h
    w = width img
    h = height img
    num = fromIntegral num'
    alpha = (2*pi)/num
    root2 = sqrt (2.0)

getMask ::
     Int
  -> MaskParams
  -> Diagram Rasterific
  -> ByteString
getMask width (MaskParams dilValue blurVal) dia =
  enc blurredFinalMask

  where
    enc i = BSL.toStrict $ encodePng i

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

    blurredFinalMask = toJuicyGrey $
      blur 4 (toFridayGrey subtractedJpData)

    w :: Double
    w = fromIntegral width

render ::
     Diagram Rasterific
  -> Int
  -> JP.Image JP.PixelRGBA8
render dia width = renderDia Rasterific
          (RasterificOptions (mkSizeSpec2D (Just w) (Just w)))
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
      (\x y -> JP.pixelAt img (x + xoff) (y + yoff)) edge edge

    edge = min wi hi
    wi = (JP.imageWidth img)
    hi = (JP.imageHeight img)

    xoff = if wi > hi then (ceiling ((fromIntegral (wi - edge))/2.0)) else 0
    yoff = if wi < hi then (ceiling ((fromIntegral (hi - edge))/2.0)) else 0

-- createFrame ::
--      BackgroundImage
--   -> ForeGround
--   -> Mask
--   -> Int
--   -> ByteString

-- createFrame img fg mask width =
--   BSL.toStrict $ encodePng maskedImgJpData
--   where
--     -- Create the raster of all three of same size
--     -- Make the mask portion invisible for image
--     -- Add the frame on top of this

--     imgJP = renderSquareImage (origBackgroundImage img) width
--     fgJP = render (foreGroundDia fg) width


--     (_,_,_,msk) = getMask (foreGroundDia fg)
--             width (maskParams mask)

--     maskJP = JP.pixelMap modMask $ msk

--     modMask :: JP.Pixel8 -> JP.PixelRGBA8
--     -- Black portion, outside
--     modMask 0 = JP.PixelRGBA8 0 0 0 0
--     -- White portion, inside
--     modMask a = JP.PixelRGBA8 255 255 255 a

--     maskedImgJpData =
--       JP.zipPixelComponent3 doMasking
--         imgJP fgJP maskJP

--     doMasking i f m =
--       if m > 0
--         -- Blur orig Image at the border
--         then ceiling $
--           ((fromIntegral i)* (fromIntegral m))/255
--         else f

getForeGround ::
     NonEmpty (Diagram Rasterific)
  -> NonEmpty ForeGroundParams
  -> Diagram Rasterific
getForeGround
  imgs layers
  = mconcat $ NE.toList $ NE.zipWith getForeGroundLayer imgs layers

getForeGroundLayer ::
     Diagram Rasterific
  -> ForeGroundParams
  -> Diagram Rasterific
getForeGroundLayer img
  (ForeGroundParams num rotOffset scaling radiusOffset)
  = mconcat $ map snd finalList
  where
    tmplt = Horizontal
    initRot =
      case tmplt of
        Horizontal -> 0
        Vertical -> -90
        Diagnol -> -45

    radius = getDefaultRadius num tmplt img
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

    radiusAfterOffset = radius * (radiusOffset/100)
    -- Apply transformations
    transList =
      map (\(n,i) -> (n, translateX (x n) (translateY (y n) i))) rotOffsetApplied
        where
          x n = g sin n
          y n = g cos n
          g f n = radiusAfterOffset*f
            ((((-2)*(fromIntegral n))/(fromIntegral num))*pi)
