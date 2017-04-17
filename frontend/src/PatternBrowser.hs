{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
module PatternBrowser where

import Reflex.Dom

import Utils

import qualified Data.Map as Map

import Data.Text (Text)
import qualified Data.Text                 as T
import qualified Data.Text.Encoding        as T
import Data.Monoid
import Data.Aeson
import Data.Maybe
import Control.Monad

import Message

patternBrowseWidget fullHost patternListEv = do

  let
    getImgUrl grp file = "http://" <> fullHost
      <> "/static/patterns/" <> grp <> "/" <> file

    f (groupName, files) = do
      el "ul" $ do
        text groupName
        forM files
          (\file -> do
              el "li" $
                elAttr "img"
                  ("src" =: getImgUrl groupName file) blank
          )

    getList = (\(PatternList lst) -> lst) <$> patternListEv

  widgetHold (text "loading") ((mapM_ f) <$> getList)

  ev <- getPostBuild
  return $ enc (GetPatternList <$ ev)
