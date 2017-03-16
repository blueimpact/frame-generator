{-# LANGUAGE DeriveGeneric #-}

-- Common code between front-end and back-end
module Common where

import GHC.Generics
import Data.Aeson

-- Edit ForeGround, Client Request Data
data ClientReqEditFG = ClientReqEditFG {
    clientReqEditFGCount      :: Int
  , clientReqEditFGRotation   :: Double -- In Degrees
  , clientReqEditFGScaling    :: Double
  , clientReqEditFGRadiusOff  :: Double -- %age
}
  deriving (Generic, Show)

instance ToJSON ClientReqEditFG where
    -- No need to provide a toJSON implementation.

    -- For efficiency, we write a simple toEncoding implementation, as
    -- the default version uses toJSON.
    toEncoding = genericToEncoding defaultOptions

instance FromJSON ClientReqEditFG
    -- No need to provide a parseJSON implementation.
