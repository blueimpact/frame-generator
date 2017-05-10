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

      wsSend = leftmost [enc navReq, req1, req2, req4
                        , req6, req7]

    patListDyn <- holdDyn [] patternList
    fgtListDyn <- holdDyn [] fgtList

    navReq <- navbar

    req1 <- patternBrowseWidget fullHost patListDyn

    req4 <- foreGroundTemplateBrowseWidget fullHost fgtList

    req2 <- editFGTemplateWidget fullHost patListDyn
      (getResponse ws) (getResponse ws)

    req6 <- foreGroundBrowseWidget fullHost (getResponse ws) (getResponse ws)
    req7 <- previewWidget fullHost patternList fgtListDyn (getResponse ws)

    ws <- webSocket url $ def &
      webSocketConfig_send .~ wsSend
    putDebugLnE (_webSocket_recv ws) show
    putDebugLnE wsSend show
    putDebugLnE (patternList) show
    putDebugLnE (fgtList) show
  return ()

navbar :: (MonadWidget t m)
  => m (Event t Message.Request)
navbar = do
  elClass "nav" "navbar navbar-default navbar-fixed-top" $
    divClass "container" $ do
      divClass "navbar-header" $ do
        elAttr "a" (("class" =: "navbar-brand") <> ("href" =: "#")) $
          text "Frame Generator"
      divClass "navbar-collapse collapse" $
        elClass "ul" "nav navbar-nav" $ do
          elClass "li" "" $ elAttr "a" ("href" =: "#pattern_browser") $ text "Patterns"
          e1 <- elClass "li" "" $ do
            (e,_) <- elAttr' "a" ("href" =: "#template_browser") $ text "Templates"
            return (Message.GetForeGroundTemplateList <$ (domEvent Click e))
          e2 <- elClass "li" "" $ do
            (e,_) <- elAttr' "a" ("href" =: "#foreground_browser") $ text "ForeGrounds"
            return (Message.GetForeGroundList <$ (domEvent Click e))
          e3 <- elClass "li" "" $ do
            (e,_) <- elAttr' "a" ("href" =: "#preview_widget") $ text "Preview Widget"
            return never
          elClass "li" "" $
            elAttr "a" ("href" =: "/uploadpatterns") $ text "Upload Patterns"
          return $ leftmost [e1,e2,e3]


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
