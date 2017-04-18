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
import Data.Maybe (isJust)
import qualified Data.Set as S
import Data.Aeson
import Data.Maybe
import Control.Monad
import Reflex.Dom.Contrib.Utils
import Message

patternBrowseWidget fullHost patternListEv = do

  let
    getImgUrl grp file = "http://" <> fullHost
      <> patternsDir <> grp <> "/" <> file

    f (groupName, files) = do
      el "ul" $ do
        text groupName
        forM files
          (\file ->
              el "li" $ do
                text file
                ev <- buttonE "Create Template" $
                  CreateForeGroundTemplate (groupName, file)
                img $ getImgUrl groupName file
                return ev
          )

  evDyn <- el "div" $
    widgetHold (do {text "loading"; return [];})
             ((mapM f) <$> patternListEv)

  let evClick = switchPromptlyDyn (leftmost <$> evDyn)

  ev <- getPostBuild
  return $ enc $ leftmost [(GetPatternList <$ ev), evClick]

foreGroundTemplateBrowseWidget fullHost fgTListEv = do
  let

    f fgtId = do
      el "li" $ do
        preview <- buttonE "Preview" $
          PreviewForeGroundTemplate fgtId []
        edit <- buttonE "Edit" $
          EditForeGroundTemplate fgtId
        clone <- buttonE "Clone" $
          CloneForeGroundTemplate fgtId
        delete <- buttonE "Delete" $
          DeleteForeGroundTemplate fgtId
        let url = "http://" <> fullHost
              <> fgtemplatesDir <> showt fgtId <> ".png"
        img url

        return $ leftmost $
          [preview, edit, clone, delete]

  evDyn <- el "div" $
    widgetHold (do {return [];})
             ((mapM f) <$> fgTListEv)

  let evClick = switchPromptlyDyn (leftmost <$> evDyn)

  return $ enc evClick

previewWidget fullHost patListDyn fgtListDyn fgPreviewListEv = do

  let
    getList = (\(ForeGroundListPreview lst) -> lst) <$> fgPreviewListEv
    f (fgtId, pats, file) = do
      el "li" $ do
        let url = "http://" <> fullHost
              <> previewDir <> file
        img url
        buttonE "Save" $
          ApplyForeGroundTemplate fgtId pats

    fgtBrowse fgtId grp = do
      el "li" $ do
        let url = "http://" <> fullHost
              <> fgtemplatesDir <> showt fgtId <> ".png"
        img url

        let pats = concat $ ffor grp (\(g,fs) ->
                     ffor fs (\f -> (g,f))

        buttonE "Select" $
          PreviewForeGroundTemplate fgtId pats

    filtFun t (g,_) = isJust (T.commonPrefixes t g)

  evDyn <- el "div" $
    el "ul" $ do
      grpSelDyn <- el "li" $
        -- Group list, checkbox
        ti <- textInput def
        cblW <- dyn $ (checkboxList fst
          filtFun
          never
          (value ti)
          S.empty)
          <$> patListDyn

        return $ value cblW

      el "li" $ el "ul" $
        -- template list, select
        dyn $ zipDynWith fgtBrowse fgtListDyn grpSelDyn

      el "li" $ do
        -- preview pane

        el "div" $ el "ul" $
          widgetHold (do {return [];})
                   ((mapM f) <$> getList)

  let evClick = leftmost $
        map switchPromptlyDyn evDyn

  return $ enc evClick


foreGroundBrowseWidget fullHost fgListEv = do
  let
    getList = (\(ForeGroundList lst) -> lst) <$> fgTListEv

    f (fgId, file) = do
      el "li" $ do
        edit <- buttonE "Edit" $
          EditForeGround fgId
        delete <- buttonE "Delete" $
          DeleteForeGround fgId
        -- Edit mask
        -- Download
        let url = "http://" <> fullHost
              <> foregroundDir <> file
        img url

        return $ leftmost $
          [edit, delete]
  evDyn <- el "div" $
    widgetHold (do {return [];})
             ((mapM f) <$> getList)

  let evClick = switchPromptlyDyn (leftmost <$> evDyn)

  return $ enc evClick
