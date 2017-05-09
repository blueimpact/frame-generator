{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}
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
import Data.Set (Set)
import qualified Data.Set as Set
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

patternBrowseWidget fullHost patListDyn = do

  let
    getImgUrl grp file = "http://" <> fullHost
      <> patternsDir <> grp <> "/" <> file

    f :: (MonadWidget t m) => (Text,[Text]) -> m (Event t Message.Request)
    f (groupName, files) = do
      divClass "row panel panel-primary" $ do
        divClass "panel-heading" $ text groupName
        eVs <- forM files
          (\file ->
             divClass "col-md-1" $ do
               text file
               e <- imgJump (Just "#edit-fgt-widget")
                 $ getImgUrl groupName file
               return $
                 (CreateForeGroundTemplate (groupName, file))
                  <$ domEvent Click e
          )
        return (leftmost eVs)

    widget patList = do
      eVs <- (mapM f patList)
      return $ leftmost (eVs)


  evClick <- divClass "container" $ idTag "pattern_browser" $
    divClass "row panel panel-primary" $ do
      divClass "panel-heading" $ text "Patterns"
      divClass "panel-body container" $
        doPagination 3 patListDyn widget

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
        imgJump (Nothing) url

        return $ leftmost $
          [preview, edit, clone, delete]

  evDyn <- divClass "container" $ idTag "template_browser" $
    divClass "panel panel-primary" $ do
      divClass "panel-heading" $ text "Templates"
      divClass "panel-body" $
        widgetHold (do {return [];})
             ((mapM f) <$> fgTListEv)

  let evClick = switchPromptlyDyn (leftmost <$> evDyn)

  return $ enc evClick

previewWidget :: forall t m .
  (MonadWidget t m)
  => Text
  -> Event t [(Text,[Text])]
  -> Dynamic t [FgtId]
  -> Event t (Message.Response ForeGroundListPreviewT)
  -> m (Event t [ByteString])
previewWidget fullHost patternList fgtListDyn fgPreviewListEv = do

  let
    getList = (\(ForeGroundListPreview lst) -> lst) <$> fgPreviewListEv

    filtFun t (g,_) =
      if T.null t
        then True
        else isJust (T.commonPrefixes (T.toLower t) (T.toLower g))

    -- Show Preview
    f (fgtId, pats, file) = do
      divClass "col-md-2" $ do
        let url = "http://" <> fullHost
              <> file
        img url
        eDyn <- widgetHold (buttonE "Save" $
          ApplyForeGroundTemplate fgtId pats) (never)
        return $ switchPromptlyDyn eDyn

    -- Show Templates
    fgtBrowse :: (MonadWidget t m)
      => ([(Text,[Text])], [(Text,[Text])], [(Text,[Text])])
      -> FgtId
      -> m (Event t Message.Request)
    fgtBrowse (l1,l2,l3) fgtId = do
      divClass "col-md-2" $ do
        let url = "http://" <> fullHost
              <> fgtemplatesDir <> tshow fgtId <> ".png"
        e <- imgJump (Just "#preview") url

        let pats grp = NE.nonEmpty $ concat $ concat $ ffor grp (\(g,fs) ->
                     ffor fs (\f -> [(g,f)]))
            l1Pats = pats l1
            l2Pats = pats l2
            l3Pats = pats l3

        let ev = fforMaybe (domEvent Click e)
                   (\_ -> (PreviewForeGroundTemplate fgtId <$> l1Pats
                     <*> pure l2Pats <*> pure l3Pats))
        return ev

    fgtBrowse' fgtIds grp = mapM (fgtBrowse grp) fgtIds

    -- Show selector for 3 layers
    layerPatternGroupSelector :: (MonadWidget t m)
      => m (Dynamic t ([(Text,[Text])],[(Text,[Text])],[(Text,[Text])]))
    layerPatternGroupSelector = do
      divClass "row" $ el "table" $ el "tr" $ do
        (grpSelDyn1 :: Dynamic t (Dynamic t [(Text,[Text])]))
          <- elClass "td" "pre-scrollable" $ do
            -- Group list, checkbox
            text "Layer 1"
            ti <- textInput def
            widgetHold (return (constDyn [])) $ (\pats -> do
              w <- checkboxList fst
                filtFun
                never
                (value ti)
                Set.empty
                pats
              return $ value w) <$> patternList

        grpSelDyn2 <- elClass "td" "pre-scrollable" $ do
          -- Group list, checkbox
          text "Layer 2"
          ti <- textInput def
          widgetHold (return (constDyn [])) $ (\pats -> do
            w <- checkboxList fst
              filtFun
              never
              (value ti)
              Set.empty
              pats
            return $ value w) <$> patternList

        grpSelDyn3 <- elClass "td" "pre-scrollable" $ do
          -- Group list, checkbox
          text "Layer 3"
          ti <- textInput def
          widgetHold (return (constDyn [])) $ (\pats -> do
            w <- checkboxList fst
              filtFun
              never
              (value ti)
              Set.empty
              pats
            return $ value w) <$> patternList
        return $ zipDynWith (\a (b,c) -> (a,b,c)) (join grpSelDyn1)
          (zipDyn (join grpSelDyn2) (join grpSelDyn3))

  -- Widget Code
  evClick <- divClass "preview-widget container" $ idTag "preview_widget" $
    divClass "panel panel-primary" $ do
      divClass "panel-heading" $ text "Preview Widget"
      divClass "panel-body" $ do
        grpSelDyn <- layerPatternGroupSelector

        -- Select new template event
        ev1' <- divClass "row" $
          -- template list, select
          dyn $ zipDynWith fgtBrowse' fgtListDyn grpSelDyn

        divClass "h2" $ idTag "preview" $ text "Previews"
        -- Apply template event
        ev2Dyn <- divClass "row" $ do
          -- preview pane

          divClass "" $
            widgetHold (do {return [];})
              (sequence <$> ((map f) <$> getList))

        ev1 <- switchPromptly never (leftmost <$> ev1')
        let
          ev2 :: (Reflex t) => Event t Message.Request
          ev2 = switchPromptlyDyn $ leftmost <$> ev2Dyn
        return $ leftmost [ev1,ev2]

  return $ enc evClick

foreGroundBrowseWidget :: forall t m .
  (MonadWidget t m)
  => Text
  -> Event t (Message.Response ForeGroundListT)
  -> Event t (Message.Response DownloadForeGroundPngLinkT)
  -> m (Event t [ByteString])
foreGroundBrowseWidget fullHost fgListEv downloadLink = do
  let
    getList = (\(ForeGroundList lst) -> lst) <$> fgListEv

    f :: (MonadWidget t m) =>
         Event t Bool
      -> (FgId, Text)
      -> m (Event t Message.Request, Event t (FgId,Bool))
    f selEv (fgId, file) = do
      divClass "col-md-2" $ do
        cb <- checkbox False $ def & setValue .~ selEv

        edit <- buttonE "Edit" $
          EditForeGround fgId
        delete <- buttonE "Delete" $
          DeleteForeGround [fgId]
        -- Edit mask
        -- Download
        let url = "http://" <> fullHost
              <> file
        img url

        return $ (leftmost [edit, delete],
                  (\b -> (fgId,b)) <$> _checkbox_change cb)

  ev <- divClass "container" $ idTag "foreground_browser" $
    divClass "panel panel-primary" $ do
      divClass "panel-heading" $ text "ForeGrounds"
      (selEv, download, deleteSel) <- divClass "panel-body row" $ do

        selectEv <- button "Select All"
        selectDyn <- foldDyn (\_ b -> not b) (False) selectEv

        download <- button "Download"
        deleteSel <- button "Delete Selected"

        let selEv = updated selectDyn
        return (selEv, download, deleteSel)

      widgetHold blank
        (ffor downloadLink
          (\(DownloadForeGroundPngLink l) ->
            divClass "panel-body row" $
              elAttr "a" ("href" =: l) $ text "Download zip"))

      divClass "panel-body row" $ do
        (sfalsjf :: Dynamic t ([(Event t Message.Request, Event t (FgId,Bool))]))
          <- widgetHold (do {return [];})
            (sequence <$> ((map (f selEv)) <$> getList))

        let evClick = switchPromptlyDyn $ leftmost <$> ((map fst) <$> sfalsjf)
            evCBox :: Dynamic t (Event t (NonEmpty (FgId,Bool)))
            evCBox = mergeList <$> ((map snd) <$> sfalsjf)


            setSel :: Dynamic t (m (Dynamic t (Set FgId)))
            setSel = f <$> evCBox
              where
                f ev = foldDyn handler (Set.empty) ev
                handler neList set = foldl g set neList
                g set (fgId, sel) = if sel
                                      then Set.insert fgId set
                                      else Set.delete fgId set

        setDyn' <- dyn setSel
        setDyn'' <- holdDyn (constDyn Set.empty) setDyn'

        let
          setDyn :: Dynamic t (Set FgId)
          setDyn = join setDyn''
          setListDyn = Set.toList <$> setDyn
          evDelete = DeleteForeGround <$> (tagPromptlyDyn setListDyn deleteSel)
          evDownload = DownloadForeGroundPng <$> (tagPromptlyDyn setListDyn download)

        return $ leftmost [evClick, evDelete, evDownload]
  return $ enc ev
