{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE EmptyDataDecls #-}


-- Messages
module Message where

import GHC.Generics
import Data.Aeson
import Data.Text (Text)
import Data.List.NonEmpty (NonEmpty)

import Common

data Request =
    GetPatternList
  | GetForeGroundTemplateList
  | CreateForeGroundTemplate PatternName
  | EditForeGroundTemplate FgtId
  | CloneForeGroundTemplate FgtId
  | DeleteForeGroundTemplate FgtId
  | PreviewForeGroundTemplate FgtId [NonEmpty PatternName]
  | ApplyForeGroundTemplate FgtId (NonEmpty PatternName)
  | GetForeGroundList
  | EditForeGround FgId
  | DeleteForeGround FgId
  | DownloadForeGroundPng [FgId]
  deriving (Generic, Show)

-- Response Tags
data PatternListT
data ForeGroundTemplateListT
data NewForeGroundTemplateT
data ForeGroundTemplateDataT
data ForeGroundListPreviewT
data ForeGroundListT
data NewForeGroundT
data ForeGroundDataResT
data DownloadForeGroundPngLinkT

data family Response a

data instance Response PatternListT = PatternList [(Text, [Text])]
  deriving (Generic, Show)

data instance Response ForeGroundTemplateListT =
  ForeGroundTemplateList [FgtId]
  deriving (Generic, Show)

data instance Response NewForeGroundTemplateT =
  NewForeGroundTemplate FgtId
  deriving (Generic, Show)

data instance Response ForeGroundTemplateDataT =
  ForeGroundTemplateDataRes FgtId ForeGroundTemplateData
  deriving (Generic, Show)

data instance Response ForeGroundListPreviewT =
  ForeGroundListPreview [(FgtId, NonEmpty PatternName, FileName)]
  deriving (Generic, Show)

data instance Response ForeGroundListT =
  ForeGroundList [(FgId, FileName)]
  deriving (Generic, Show)

data instance Response NewForeGroundT =
  NewForeGround FgId -- response to ApplyForeGroundTemplate
  deriving (Generic, Show)

data instance Response ForeGroundDataResT =
  ForeGroundDataRes ForeGroundData
  deriving (Generic, Show)

data instance Response DownloadForeGroundPngLinkT =
  DownloadForeGroundPngLink Text
  deriving (Generic, Show)


-- Client side requests
-- Edit ForeGround, Client Request Data
data EditFGTemplate =
    Edit LayerId ForeGroundParams
  | AddLayer PatternName
  | DeleteLayer LayerId
  | EditMask MaskParams
  | SaveFG
  | SaveMask
  deriving (Generic, Show)

data EditPane =
    GetFGDefaultParams
  | GetMaskDefaultParams
  deriving (Generic, Show)

instance ToJSON Request where
    toEncoding = genericToEncoding defaultOptions

instance FromJSON Request

instance ToJSON (Response PatternListT) where
    toEncoding = genericToEncoding defaultOptions

instance FromJSON (Response PatternListT)

instance ToJSON (Response ForeGroundTemplateListT) where
    toEncoding = genericToEncoding defaultOptions

instance FromJSON (Response ForeGroundTemplateListT)

instance ToJSON (Response NewForeGroundTemplateT) where
    toEncoding = genericToEncoding defaultOptions

instance FromJSON (Response NewForeGroundTemplateT)

instance ToJSON (Response ForeGroundTemplateDataT) where
    toEncoding = genericToEncoding defaultOptions

instance FromJSON (Response ForeGroundTemplateDataT)

instance ToJSON (Response ForeGroundListPreviewT) where
    toEncoding = genericToEncoding defaultOptions

instance FromJSON (Response ForeGroundListPreviewT)

instance ToJSON (Response ForeGroundListT) where
    toEncoding = genericToEncoding defaultOptions

instance FromJSON (Response ForeGroundListT)

instance ToJSON (Response NewForeGroundT) where
    toEncoding = genericToEncoding defaultOptions

instance FromJSON (Response NewForeGroundT)

instance ToJSON (Response ForeGroundDataResT) where
    toEncoding = genericToEncoding defaultOptions

instance FromJSON (Response ForeGroundDataResT)

instance ToJSON (Response DownloadForeGroundPngLinkT) where
    toEncoding = genericToEncoding defaultOptions

instance FromJSON (Response DownloadForeGroundPngLinkT)

instance ToJSON EditPane where
    toEncoding = genericToEncoding defaultOptions
instance FromJSON EditPane

instance ToJSON EditFGTemplate where
    -- No need to provide a toJSON implementation.

    -- For efficiency, we write a simple toEncoding implementation, as
    -- the default version uses toJSON.
    toEncoding = genericToEncoding defaultOptions

instance FromJSON EditFGTemplate
    -- No need to provide a parseJSON implementation.
