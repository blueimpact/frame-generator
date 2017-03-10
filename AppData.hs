{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module AppData
  where

import Data.Map (Map)
import Control.Concurrent.MVar
import Diagrams.Core.Types
import Data.ByteString
import Diagrams.Backend.Rasterific
import Web.PathPieces

newtype PatternID = PatternID { unPatternID :: Int}
  deriving (Read, PathPiece, Show, Eq, Ord)

data ForeGroundTemplate =
    Horizontal
  | Vertical
  | Diagnol

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

data ForeGroundData = ForeGroundData {
    pattern         :: Diagram Rasterific
  , foreGround      :: MVar ForeGround
}

-- Generated PNG DB
newtype PngID = PngID { unPngID :: Int}
  deriving (Read, PathPiece, Show, Eq, Ord)

data App = App {
    patternDB       :: MVar (Map PatternID PatternData)
  , foreGroundDB    :: MVar (Map ForeGroundID ForeGroundData)
  , pngDB           :: MVar (Map PngID ByteString)
}
