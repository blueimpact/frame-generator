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
  , template        :: ForeGroundTemplate
  , defaultRadius   :: Double
  , defaultCount    :: Int
  --, previewPng      :: Maybe ByteString
}

newtype ForeGroundID = ForeGroundID { unForeGroundID :: Int}
  deriving (Read, PathPiece, Show, Eq, Ord)

data ForeGroundData = ForeGroundData {
    pattern         :: MVar PatternID
  , patternCount    :: MVar Int
  , rotationOffset  :: MVar Double
  , scaling         :: MVar Double
  , radiusOffset    :: MVar Double
}

-- Generated PNG DB
newtype PngID = PngID { unPngID :: Int}
  deriving (Read, PathPiece, Show, Eq, Ord)

data App = App {
    patternDB       :: MVar (Map PatternID PatternData) 
  , foreGroundDB    :: MVar (Map ForeGroundID ForeGroundData)
  , pngDB           :: MVar (Map PngID ByteString)
}
