{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}

module Lib where

import Data.FileEmbed
import Reflex.Dom

import Utils
import EditFGTemplate
import PatternBrowser

import qualified Data.Map as Map

import qualified Reflex.Dom.Contrib.Router as C
import qualified URI.ByteString            as U
import           Control.Lens              ((&), (.~), (^.), view, _Just)
import qualified Data.Text                 as T
import qualified Data.Text.Encoding        as T
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Monoid
import Data.Aeson
import Data.Maybe
import Control.Monad.IO.Class
import Reflex.Dom.Contrib.Utils

import Common
import qualified Message

main = mainWidgetWithCss
  ($(embedFile "src/style.css")
  <> $(embedFile "src/bootstrap.css"))
  mainWidgetTop

mainWidgetTop :: (MonadWidget t m
   , PerformEvent t m, Reflex t) => m ()
mainWidgetTop = do
  fullHost <- getFullHostUrl

  let
    url = "ws://" <> fullHost <> "/websocket"

  rec
    let
      patternList = (\(Message.PatternList lst) -> lst) <$>
        (getResponse ws)

      fgtList = (\(Message.ForeGroundTemplateList lst) -> lst) <$>
        (getResponse ws)

      wsSend = leftmost [req1, req2, enc req3, req4
                        , enc req5, req6, req7]

    patListDyn <- holdDyn [] patternList
    fgtListDyn <- holdDyn [] fgtList

    req1 <- patternBrowseWidget fullHost patternList

    req3 <- buttonE "Get FGT List" Message.GetForeGroundTemplateList

    req4 <- foreGroundTemplateBrowseWidget fullHost fgtList

    req2 <- editFGTemplateWidget fullHost patListDyn
      (getResponse ws) (getResponse ws)

    req5 <- buttonE "Get FG List" Message.GetForeGroundList

    req6 <- foreGroundBrowseWidget fullHost (getResponse ws)
    req7 <- previewWidget fullHost patternList fgtListDyn (getResponse ws)

    ws <- webSocket url $ def &
      webSocketConfig_send .~ wsSend
    putDebugLnE (_webSocket_recv ws) show
    putDebugLnE wsSend show
    putDebugLnE (patternList) show
    putDebugLnE (fgtList) show
  return ()

getResponse ws =
  fforMaybe (_webSocket_recv ws)
    Message.getResponse

getFullHostUrl :: (HasWebView m, MonadIO m) => m T.Text
getFullHostUrl = do
  uri <- C.getURI

  let
    -- Get the host name + port
    fullHost = if T.null port then host else host <> ":" <> port

    hostMaybe = ffor authority (\x -> T.decodeUtf8 $ U.hostBS $ x ^. U.authorityHostL)

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
