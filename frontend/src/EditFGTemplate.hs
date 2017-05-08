{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeFamilies #-}
module EditFGTemplate where

import Reflex.Dom

import Utils

import qualified Data.Map as Map

import Data.Text (Text)
import qualified Data.Text                 as T
import qualified Data.Text.Encoding        as T
import Data.Semigroup
import Data.Aeson
import Data.Maybe
import Control.Lens
import Control.Monad
import Control.Monad.IO.Class
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NE
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import Reflex.Dom.Contrib.Utils

import qualified CSSClass as C
import Common
import Message

-- type FgtData = [(PatternName, ForeGroundParams)]
editFGTemplateWidget ::
  (MonadWidget t m)
  => Text
  -> Dynamic t ([(Text,[Text])])
  -> Event t (Message.Response NewForeGroundTemplateT)
  -> Event t (Message.Response ForeGroundTemplateDataT)
  -> m (Event t [ByteString])
editFGTemplateWidget fullHost patListDyn newFGTemplEv fgtDataEv' = do

  let
    fgtIdEv = (\(NewForeGroundTemplate fgtId) -> fgtId) <$> newFGTemplEv

    fgtDataEv = (\(ForeGroundTemplateDataRes fgtId fgtData) ->
                   (fgtId, fgtData)) <$> fgtDataEv'

    editFGTEv = EditForeGroundTemplate <$> fgtIdEv

    f d = divClass "container" $ do
      divClass "panel panel-primary" $ do
        divClass "panel-heading" $
          text "Edit Widget"
        divClass "panel-body" $
          renderEditWidget fullHost patListDyn d

  evDyn <- idTag "edit-fgt-widget" $
    widgetHold (do {return never;}) (f <$> fgtDataEv)

  let evClick = switchPromptlyDyn evDyn

  return $ enc $ leftmost [editFGTEv, evClick]

-- On save
--   - Send SaveFG to server
--   - Capture controls value for reset
-- On Add Layer
--   - Send AddLayer to server
--   - Add a layer to local fgtDataDyn
--
renderEditWidget ::
  (MonadWidget t m)
  => Text
  -> Dynamic t ([(Text,[Text])])
  -> (FgtId, ForeGroundData)
  -> m (Event t Message.Request)
renderEditWidget fullHost pats
  (fgtId, (ForeGroundData fgtData)) = do
  rec
    let eventMessage = enc $ leftmost [ev1,ev2]

        idTxt = tshow fgtId
        ev2 = SaveFG <$ save
        editFGTEv = EditForeGroundTemplate fgtId <$
          (leftmost [reset])
    ws <- webSocket ("ws://" <> fullHost <> "/edit/foreground/" <> idTxt) $
      def & webSocketConfig_send .~ eventMessage


    -- Controls
    ev1 <- elClass "table" "table" $
      elClass "tr" "" $ do
        rec
          let lDyn = NE.toList <$> ((NE.zip (NE.fromList [1..])) <$> fgtDataDyn)
              ev = leftmost $ [addLayerMsg, ev2]
              ev2 = switchPromptlyDyn $ leftmost <$> editMsgs
              handler (AddLayer pat) d = Just $ d <>
                NE.fromList [(pat,def :: ForeGroundParams)]
              handler (DeleteLayer layerId) d = NE.nonEmpty $
                (NE.take (layerId - 1) d) ++ (NE.drop layerId d)
              handler (Edit layerId params) d = if newD /= d then Just newD else Nothing
                where newD = Control.Lens.imap
                        (\i a@(p,_) -> if (i + 1) == layerId
                          then (p,params)
                          else a) d
              handler _ _ = Nothing
              lc d = do
                e <- dyn (layerControls save <$> d)
                switchPromptly never e

          fgtDataDyn <- foldDynMaybe handler fgtData ev

          editMsgs <- simpleList lDyn lc
          -- Select pattern and add a layer
          addLayerMsg <- miniPatternBrowser fullHost pats

        return ev

    -- Preview
    divClass "panel" $ do
      let
        myImgUrl =
          ffor (_webSocket_recv ws)
            (\bs -> liftIO $ createObjectURL bs)
      urlEv <- performEvent myImgUrl
      urlDyn <- holdDyn "dummy" urlEv
      let dynAttr = ffor urlDyn (\u -> ("src" =: u))
      divClass "center-block" $ elDynAttr "img" dynAttr $ return ()

    reset <- button "Reset All"
    -- Race in save signal, both WS fire
    save <- button "Save"

  return editFGTEv

miniPatternBrowser ::
  (MonadWidget t m)
  => Text
  -> Dynamic t [(Text,[Text])]
  -> m (Event t EditFGTemplate)
miniPatternBrowser fullHost pats = do
  let
    f :: (MonadWidget t m) =>
      (Text, [Text]) -> m [Event t EditFGTemplate]
    f (groupName, files) = do
      divClass "row" $ do
        forM files
          (\file ->
              divClass "col-md-1" $ do
                let
                  getImgUrl :: Text
                  getImgUrl = "http://" <> fullHost
                      <> patternsDir <> groupName <> "/" <> file
                e <- img $ getImgUrl
                return $ (AddLayer (groupName, file))
                  <$ domEvent Click e
          )
  ev <- divClass C.editFgtWidgetPatternBrowse $ dyn $ sequence <$> ((map f) <$> pats)
  let
    evFlattened = leftmost <$> (concat <$> ev)

  switchPromptly never evFlattened


layerControls ::
  (MonadWidget t m)
  => Event t ()
  -> (LayerId, (PatternName, ForeGroundParams))
  -> m (Event t EditFGTemplate)
layerControls save (layerId, (_, fgParam)) = do
  el "td" $ do
    el "table" $ do
      rec
        let
          updateResetEv = save

        s <- rangeInputWidgetWithTextEditAndReset
          "Scale:" (scaling fgParam)
            (0.1, 2.0, 0.01) updateResetEv

        c <- rangeInputWidgetWithTextEditAndReset
          "Count:" (fromIntegral (patternCount fgParam))
            (2, 128, 1) updateResetEv

        ro <- rangeInputWidgetWithTextEditAndReset
          "Rotation:" (rotationOffset fgParam)
            (-180, 180, 1) updateResetEv

        ra <- rangeInputWidgetWithTextEditAndReset
          "Radius:" (radiusOffset fgParam)
            (1, 200, 1) updateResetEv

        ao <- rangeInputWidgetWithTextEditAndReset
          "Angle:" (angleOffset fgParam)
            (-0.5, 0.5, 0.01) updateResetEv

      ev <- getPostBuild
      delete <- button "Delete Layer"
      return $ leftmost [getEditMessage layerId (s,c,ro,ra,ao) ev
               , DeleteLayer layerId <$ delete]

getEditMessage :: (Reflex t)
  => LayerId
  -> (RangeInput t, RangeInput t, RangeInput t, RangeInput t, RangeInput t)
  -> Event t ()
  -> Event t EditFGTemplate
getEditMessage layerId (scale, count, rotate, radius, angle) pb =
  Edit layerId <$> anyEditEvent
  where
    anyEditEvent = leftmost [ev1, ev2, ev3, ev4, ev5, pbEv]

    pbEv = fst <$> attachPromptlyDyn params pb

    ev1 = attachPromptlyDynWith f params sEv
      where f p x = p { scaling = x}

    ev2 = attachPromptlyDynWith f params cEv
      where f p x = p { patternCount = x}

    ev3 = attachPromptlyDynWith f params roEv
      where f p x = p { rotationOffset = x}

    ev4 = attachPromptlyDynWith f params raEv
      where f p x = p { radiusOffset = x}

    ev5 = attachPromptlyDynWith f params aoEv
      where f p x = p { angleOffset = x}

    sEv  = ftod $ updated (_rangeInput_value scale)
    cEv  = ceiling <$> updated (_rangeInput_value count)
    roEv = ftod $ updated (_rangeInput_value rotate)
    raEv = ftod $ updated (_rangeInput_value radius)
    aoEv = ftod $ updated (_rangeInput_value angle)

    params = ForeGroundParams
      <$> (ceiling <$> _rangeInput_value count)
      <*> ftod (_rangeInput_value rotate)
      <*> ftod (_rangeInput_value scale)
      <*> ftod (_rangeInput_value radius)
      <*> ftod (_rangeInput_value angle)

    ftod f = fmap realToFrac f
