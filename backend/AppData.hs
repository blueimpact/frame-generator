{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module AppData
  where

import ClassyPrelude
import Data.Int
import Data.Either
import Data.Map (Map)
import Control.Concurrent.MVar
import Diagrams.Core.Types
import Data.ByteString
import Diagrams.Backend.Rasterific
import Web.PathPieces
import qualified Codec.Picture.Types as JP

previewSize :: Int
previewSize = 600

newtype PatternID = PatternID { unPatternID :: Int}
  deriving (Read, PathPiece, Show, Eq, Ord)

data ForeGroundTemplate =
    Horizontal
  | Vertical
  | Diagnol
  deriving (Show, Eq, Enum, Bounded)

data PatternData = PatternData {
    origPatternData :: Diagram Rasterific
  , origTemplate    :: ForeGroundTemplate
  , defaultRadius   :: Double
  , defaultCount    :: Int
  --, previewPng      :: Maybe PngID
}

newtype ForeGroundID = ForeGroundID { unForeGroundID :: Int}
  deriving (Read, PathPiece, Show, Eq, Ord)

data ForeGroundParams = ForeGroundParams {
    patternCount    :: Int
  , radius          :: Double
  , rotationOffset  :: Double -- Deg
  , scaling         :: Double -- 1.0 - default
  , radiusOffset    :: Double -- %
  , template        :: ForeGroundTemplate
}

data ForeGround = ForeGround {
    foreGroundDia   :: Diagram Rasterific
  , foreGroundParams :: ForeGroundParams
  , foreGroundPng   :: PngID
}

data MaskParams = MaskParams {
    dilateValue     :: Int
  , blurValue       :: Int
}

data Mask = Mask {
    maskParams      :: MaskParams
  , maskData        :: JP.Image JP.Pixel8
}

data ForeGroundData = ForeGroundData {
    pattern         :: Diagram Rasterific
  , foreGround      :: MVar ForeGround
  , mask            :: MVar Mask
}

-- User supplied image
data BackgroundImage = BackgroundImage {
    origBackgroundImage :: Diagram Rasterific
}

newtype BackgroundImageID = BackgroundImageID { unBackgroundImageID :: Int}
  deriving (Read, PathPiece, Show, Eq, Ord)

-- Generated PNG DB
newtype PngID = PngID { unPngID :: Int}
  deriving (Read, PathPiece, Show, Eq, Ord)

data AppData = AppData {
    patternDB       :: MVar (Map PatternID PatternData)
  , foreGroundDB    :: MVar (Map ForeGroundID ForeGroundData)
  , pngDB           :: MVar (Map PngID ByteString)
  , imageDB         :: MVar (Map BackgroundImageID BackgroundImage)
}
