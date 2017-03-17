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

main = mainWidgetWithCss  $(embedFile "src/style.css") editWidgets

editWidgets :: (MonadWidget t m
   , PerformEvent t m) => m ()
editWidgets = do

  el "div" $ text "Enter the ForeGround ID"

  currentPageUrl <-textInput $ def {_textInputConfig_initialValue = "Yesod URL"}

  rec t <- textInput $ def {_textInputConfig_initialValue = "FG Id"}
      b <- button "Send"
      let
          urlValDyn = zipDyn (_textInput_value currentPageUrl) (_textInput_value t)
          newMessage = tagDyn urlValDyn $ leftmost [b, keypress Enter t]

          evMap = ffor newMessage (\t -> Map.singleton (0 ::Int) (Just t))

  el "table" $ do
    el "tr" $ do
      el "th" $ text "Edit ForeGround"
      el "th" $ text "Edit Mask"
    el "tr" $ do
      el "td" $
        editForegroundWidget evMap
      el "td" $ 
        editMaskWidget evMap
