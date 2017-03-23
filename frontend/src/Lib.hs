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

import qualified Data.Map as Map

import qualified Reflex.Dom.Contrib.Router as C
import qualified URI.ByteString            as U
import           Control.Lens              ((&), (.~), (^.), view, _Just)
import qualified Data.Text                 as T
import qualified Data.Text.Encoding        as T
import Data.Monoid

main = mainWidgetWithCss  $(embedFile "src/style.css") editWidgets

editWidgets :: (MonadWidget t m
   , PerformEvent t m) => m ()
editWidgets = do
  uri <- C.getURI
  let hostMaybe = fmap (\x -> T.decodeUtf8 $ U.hostBS $ x ^. U.authorityHostL) authority
      host = hostMaybe ^. _Just :: T.Text -- Empty string for Nothing

      portMaybe = fmap (\x -> (fmap (show.U.portNumber) (x ^. U.authorityPortL))) authority
      port = T.pack $ portMaybe ^. _Just ^. _Just

      authority = uri ^. U.authorityL
      query = (uri ^. U.queryL . U.queryPairsL)

      fgID = T.decodeUtf8 $ snd (head query)

      fullHost = if T.null port then host else host <> ":" <> port

  el "div" $ text "Edit Foreground and Mask"
  el "table" $ do
    el "tr" $ do
      el "th" $ text "Edit ForeGround"
      el "th" $ text "Edit Mask"
    el "tr" $ do
      el "td" $
        editForegroundWidget fullHost fgID
      el "td" $ 
        editMaskWidget fullHost fgID
