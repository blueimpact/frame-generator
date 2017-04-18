{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}

module Lib where

import Data.FileEmbed
import Reflex.Dom

import EditForeGround
import EditMask
import Utils
import PatternBrowser

import qualified Data.Map as Map

import qualified Reflex.Dom.Contrib.Router as C
import qualified URI.ByteString            as U
import           Control.Lens              ((&), (.~), (^.), view, _Just)
import qualified Data.Text                 as T
import qualified Data.Text.Encoding        as T
import Data.Monoid
import Data.Aeson
import Data.Maybe
import Control.Monad.IO.Class

import Common
import qualified Message

main = mainWidgetWithCss  $(embedFile "src/style.css")
  mainWidgetTop

mainWidgetTop :: (MonadWidget t m
   , PerformEvent t m) => m ()
mainWidgetTop = do
  fullHost <- getFullHostUrl

  rec
    let
      url = "ws://" <> fullHost <> "/websocket"
      wsSend = leftmost [req1, req2, req3, req4]

      getResponse =
        fforMaybe (_webSocket_recv ws)
        (\req -> case decodeStrict req of
          Just c -> Just c
          _ -> Nothing)

      -- patternList :: (Reflex t) => Event t Message.Response
      patternList = (\(PatternList lst) -> lst) <$> getResponse

      fgtList = (\(ForeGroundTemplateList lst) -> lst) <$> getResponse

    patListDyn <- holdDyn [] patternList
    fgtListDyn <- holdDyn [] fgtList

    ws <- webSocket url $ def &
      webSocketConfig_send .~ wsSend

    req1 <- patternBrowseWidget fullHost patternList

    req2 <- editFGTemplateWidget fullHost getResponse getResponse

    req3 <- buttonE "Get FGT List" GetForeGroundTemplateList

    req4 <- foreGroundTemplateBrowseWidget fullHost fgtList

    req5 <- buttonE "Get FG List" GetForeGroundList

    req6 <- foreGroundBrowseWidget fullHost getResponse

    req7 <- previewWidget fullHost patListDyn fgtListDyn getResponse

  return ()

getFullHostUrl :: (HasWebView m, MonadIO m) => m T.Text
getFullHostUrl = do
  uri <- C.getURI

  let
    -- Get the host name + port
    fullHost = if T.null port then host else host <> ":" <> port

    hostMaybe = ffor authority
      (\x -> T.decodeUtf8 $ U.hostBS $ x ^. U.authorityHostL)

    host = hostMaybe ^. _Just :: T.Text -- Empty string for Nothing

    portMaybe = ffor authority
      (\x -> fmap (show.U.portNumber) $ x ^. U.authorityPortL)

    port = T.pack $ portMaybe ^. _Just ^. _Just

    authority = uri ^. U.authorityL
    query = (uri ^. U.queryL . U.queryPairsL)

    fgID = T.decodeUtf8 $ snd (head query)
  return fullHost

-- editWidgets fullHost fgID fgParam = do

--   el "div" $
--     text "Edit Foreground and Mask"
--   el "div" $
--     text "Double click the numeric value next to slider to edit it"
--   el "table" $ do
--     el "tr" $ do
--       el "th" $ text "Edit ForeGround"
--       el "th" $ text "Edit Mask"
--     el "tr" $ do
--       el "td" $
--         editForegroundWidget fullHost fgID fgParam
--       el "td" $
--         editMaskWidget fullHost fgID
