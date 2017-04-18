{-# LANGUAGE DeriveGeneric #-}

-- Common code between front-end and back-end
module Common where

import GHC.Generics
import Data.Aeson
import Data.Text (Text)

-- Common data

type PatternName = (Text, Text)
type FileName = Text
type FgtId = Int
type FgId = Int
type LayerId = Int
type FgtData = [(PatternName, ForeGroundParams)]

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

instance ToJSON ForeGroundParams where
    toEncoding = genericToEncoding defaultOptions

instance FromJSON ForeGroundParams

instance ToJSON MaskParams where
    toEncoding = genericToEncoding defaultOptions

instance FromJSON MaskParams
