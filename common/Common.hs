{-# LANGUAGE DeriveGeneric #-}

-- Common code between front-end and back-end
module Common where

import GHC.Generics
import Data.Aeson
import Data.Text (Text)
import Data.List.NonEmpty (NonEmpty)
import Data.Int

-- Common data

type PatternName = (Text, Text)
type FileName = Text
type FgtId = Int64
type FgId = Int64
type LayerId = Int

data PatternShape = Horizontal | Vertical | Diagnol
data ForeGroundTemplateData = ForeGroundTemplateData
  (NonEmpty (PatternName, ForeGroundParams))
  deriving (Generic, Show)

data ForeGroundData = ForeGroundData FgtId
  (NonEmpty PatternName)
  deriving (Generic, Show)

data ForeGroundParams = ForeGroundParams {
    patternCount    :: Int
  , rotationOffset  :: Double -- Deg
  , scaling         :: Double -- 1.0 - default
  , radiusOffset    :: Double -- %
}
  deriving (Generic, Show, Eq)

data MaskParams = MaskParams {
    dilateValue     :: Int
  , blurValue       :: Int
}
  deriving (Generic, Show, Eq)

-- Values
fgtemplatesDir = "/static/fgtemplates/"
patternsDir = "/static/patterns/"
foregroundDir = "/static/foregrounds/"
previewDir = "/static/preview/"

instance ToJSON ForeGroundData where
    toEncoding = genericToEncoding defaultOptions

instance FromJSON ForeGroundData

instance ToJSON ForeGroundTemplateData where
    toEncoding = genericToEncoding defaultOptions

instance FromJSON ForeGroundTemplateData

instance ToJSON ForeGroundParams where
    toEncoding = genericToEncoding defaultOptions

instance FromJSON ForeGroundParams

instance ToJSON MaskParams where
    toEncoding = genericToEncoding defaultOptions

instance FromJSON MaskParams
