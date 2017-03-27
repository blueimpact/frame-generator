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

import qualified Data.Map as Map

import qualified Reflex.Dom.Contrib.Router as C
import qualified URI.ByteString            as U
import           Control.Lens              ((&), (.~), (^.), view, _Just)
import qualified Data.Text                 as T
import qualified Data.Text.Encoding        as T
import Data.Monoid
import Data.Aeson
import Data.Maybe

import Common

main = mainWidgetWithCss  $(embedFile "src/style.css") editPaneTop


-- Do some not so cool stuff
editPaneTop :: (MonadWidget t m
   , PerformEvent t m) => m ()
editPaneTop = do
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

  -- Reset all button,
  -- fetch default value and pass to the edit widgets

  -- Main edit pane web socket
  -- Get the ForeGroundParams from server
  postBuild <- getPostBuild
  let webSocketSend = enc $ const (GetFGDefaultParams) <$> postBuild
  ws <- webSocket ("ws://" <> fullHost <> "/editpane/" <> fgID) $ def &
    webSocketConfig_send .~ webSocketSend

  let maybeParams = decodeStrict' <$> _webSocket_recv ws

      defParams = ForeGroundParams 8 0 1.0 100
      params =
        (fromMaybe defParams) <$> maybeParams

      editWidgetEv = editWidgets fullHost fgID <$> params


  widgetHold (text "Loading...") editWidgetEv
  return ()

editWidgets fullHost fgID fgParam = do

  el "div" $ text "Edit Foreground and Mask"
  el "table" $ do
    el "tr" $ do
      el "th" $ text "Edit ForeGround"
      el "th" $ text "Edit Mask"
    el "tr" $ do
      el "td" $
        editForegroundWidget fullHost fgID fgParam
      el "td" $
        editMaskWidget fullHost fgID
