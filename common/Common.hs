{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

-- Common code between front-end and back-end
module Common where

import GHC.Generics
import Data.Aeson
import Data.Text (Text)
import Data.List.NonEmpty (NonEmpty)
import Data.Int
import Data.Default

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

instance Default ForeGroundParams where
  def = ForeGroundParams 8 0 1.0 100

data MaskParams = MaskParams {
    dilateValue     :: Int
  , blurValue       :: Int
}
  deriving (Generic, Show, Eq)

instance Default MaskParams where
  def = MaskParams 4 4

-- Values
fgtemplatesDir :: Text
fgtemplatesDir = "/static/fgtemplates/"

patternsDir :: Text
patternsDir = "/static/patterns/"

foregroundDir :: Text
foregroundDir = "/static/foregrounds/"

previewDir :: Text
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
