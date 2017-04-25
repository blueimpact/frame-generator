{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
module PatternBrowser where

import Reflex.Dom

import Utils
import Common
import Message

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
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NE
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import Reflex.Dom.Contrib.Utils
import Reflex.Dom.Contrib.Widgets.CheckboxList

patternBrowseWidget fullHost patternListEv = do

  let
    getImgUrl grp file = "http://" <> fullHost
      <> patternsDir <> grp <> "/" <> file

    f :: (MonadWidget t m) => (Text,[Text]) -> m [Event t Message.Request]
    f (groupName, files) = do
      divClass "row panel panel-primary" $ do
        divClass "panel-heading" $ text groupName
        forM files
          (\file ->
             divClass "col-md-1" $ do
               text file
               e <- img $ getImgUrl groupName file
               return $
                 (CreateForeGroundTemplate (groupName, file))
                  <$ domEvent Click e
          )

  evDyn <- divClass "container" $
    widgetHold (do {text "loading"; return [];}) $
      sequence <$> ((map f) <$> patternListEv)

  let
    evFlattened = leftmost <$> (concat <$> evDyn)

  let evClick = switchPromptlyDyn evFlattened
  ev <- getPostBuild
  return $ enc $ leftmost [(GetPatternList <$ ev), evClick]

foreGroundTemplateBrowseWidget fullHost fgTListEv = do
  let

    f fgtId = do
      divClass "col-md-2" $ do
        preview <- buttonE "Preview" $
          DefaultPreview fgtId
        edit <- buttonE "Edit" $
          EditForeGroundTemplate fgtId
        clone <- buttonE "Clone" $
          CloneForeGroundTemplate fgtId
        delete <- buttonE "Delete" $
          DeleteForeGroundTemplate fgtId
        let url = "http://" <> fullHost
              <> fgtemplatesDir <> tshow fgtId <> ".png"
        img url

        return $ leftmost $
          [preview, edit, clone, delete]

  evDyn <- divClass "container" $
    widgetHold (do {return [];})
             ((mapM f) <$> fgTListEv)

  let evClick = switchPromptlyDyn (leftmost <$> evDyn)

  return $ enc evClick

previewWidget ::
  (MonadWidget t m)
  => Text
  -> Event t [(Text,[Text])]
  -> Dynamic t [FgtId]
  -> Event t (Message.Response ForeGroundListPreviewT)
  -> m (Event t [ByteString])
previewWidget fullHost patternList fgtListDyn fgPreviewListEv = do

  let
    getList = (\(ForeGroundListPreview lst) -> lst) <$> fgPreviewListEv
    -- Show Preview
    f (fgtId, pats, file) = do
      divClass "col-md-2" $ do
        let url = "http://" <> fullHost
              <> file
        img url
        buttonE "Save" $
          ApplyForeGroundTemplate fgtId pats

    -- Show Templates
    fgtBrowse :: (MonadWidget t m)
      => [(Text,[Text])]
      -> FgtId
      -> m (Event t Message.Request)
    fgtBrowse grp fgtId = do
      divClass "col-md-2" $ do
        let url = "http://" <> fullHost
              <> fgtemplatesDir <> tshow fgtId <> ".png"
        e <- img url

        let pats = NE.nonEmpty $ concat $ concat $ ffor grp (\(g,fs) ->
                     ffor fs (\f -> [(g,f)]))

        let ev = fforMaybe (domEvent Click e)
                   (\_ -> PreviewForeGroundTemplate fgtId <$> pats)
        return ev

    fgtBrowse' fgtIds grp = mapM (fgtBrowse grp) fgtIds
    filtFun t (g,_) =
      if T.null t
        then True
        else isJust (T.commonPrefixes t g)

  evClick <- divClass "preview-widget container" $
    divClass "panel" $ do
      divClass "panel-heading" $ text "Preview Widget"
      divClass "panel-body" $ do

        grpSelDyn <- divClass "row" $ do
          -- Group list, checkbox
          ti <- textInput def
          widgetHold (return (constDyn [])) $ (\pats -> do
            w <- checkboxList fst
              filtFun
              never
              (value ti)
              S.empty
              pats
            return $ value w) <$> patternList

        -- Select new template event
        ev1' <- divClass "row" $
          -- template list, select
          dyn $ zipDynWith fgtBrowse' fgtListDyn (join grpSelDyn)

        divClass "h2" $ text "Previews"
        -- Apply template event
        ev2Dyn <- divClass "row" $ do
          -- preview pane

          divClass "" $
            widgetHold (do {return [];})
              (sequence <$> ((map f) <$> getList))

        ev1 <- switchPromptly never (leftmost <$> ev1')
        let ev2 = switchPromptlyDyn $ leftmost <$> ev2Dyn
        return $ leftmost [ev1,ev2]

  return $ enc evClick

foreGroundBrowseWidget ::
  (MonadWidget t m)
  => Text
  -> Event t (Message.Response ForeGroundListT)
  -> m (Event t [ByteString])
foreGroundBrowseWidget fullHost fgListEv = do
  let
    getList = (\(ForeGroundList lst) -> lst) <$> fgListEv

    f :: (MonadWidget t m) => (FgId, Text) -> m (Event t Message.Request)
    f (fgId, file) = do
      divClass "col-md-2" $ do
        edit <- buttonE "Edit" $
          EditForeGround fgId
        delete <- buttonE "Delete" $
          DeleteForeGround fgId
        -- Edit mask
        -- Download
        let url = "http://" <> fullHost
              <> file
        img url

        return $ leftmost $
          [edit, delete]
  evDyn <- divClass "container" $
    divClass "panel" $ do
      divClass "panel-heading" $ text "FG Browse Widget"
      divClass "panel-body" $ divClass "row" $ do
        widgetHold (do {return [];})
          (sequence <$> ((map f) <$> getList))

  let evClick = switchPromptlyDyn $ leftmost <$> evDyn

  return $ enc evClick
