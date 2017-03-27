{-# LANGUAGE DeriveGeneric #-}

-- Common code between front-end and back-end
module Common where

import GHC.Generics
import Data.Aeson

-- Client side requests
-- Edit ForeGround, Client Request Data
data ClientReqEditFG =
    ClientReqEditFG ForeGroundParams
  | ClientReqEditMask MaskParams
  | ClientReqSaveFG
  | ClientReqSaveMask
  deriving (Generic, Show)

instance ToJSON ClientReqEditFG where
    -- No need to provide a toJSON implementation.

    -- For efficiency, we write a simple toEncoding implementation, as
    -- the default version uses toJSON.
    toEncoding = genericToEncoding defaultOptions

instance FromJSON ClientReqEditFG
    -- No need to provide a parseJSON implementation.

data ClientReqEditPane =
    GetFGDefaultParams
  | GetMaskDefaultParams

-- Common data

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

instance ToJSON ForeGroundParams where
    toEncoding = genericToEncoding defaultOptions

instance FromJSON ForeGroundParams

instance ToJSON MaskParams where
    toEncoding = genericToEncoding defaultOptions

instance FromJSON MaskParams 
