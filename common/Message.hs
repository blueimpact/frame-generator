{-# LANGUAGE DeriveGeneric #-}

-- Messages
module Message where

import GHC.Generics
import Data.Aeson
import Data.Text (Text)

data Request =
    GetPatternList
  deriving (Generic, Show)

data Response =
    PatternList [(Text, [Text])]
  deriving (Generic, Show)

instance ToJSON Request where
    toEncoding = genericToEncoding defaultOptions

instance FromJSON Request

instance ToJSON Response where
    toEncoding = genericToEncoding defaultOptions

instance FromJSON Response
