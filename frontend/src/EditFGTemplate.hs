{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
module EditFGTemplate where

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

-- type FgtData = [(PatternName, ForeGroundParams)]
editFGTemplateWidget fullHost newFGTemplEv fgtDataEv = do

  let
    getImgUrl grp file = "http://" <> fullHost
      <> "/static/patterns/" <> grp <> "/" <> file

    fdtIdEv = (\(NewForeGroundTemplate fgtId) -> fdtId) <$> newFGTemplEv

    editFGTEv = EditForeGroundTemplate <$> fdtIdEv

    f layers = do
      el "div" $ do
        el "table" $ do
          renderEditWidget fullHost layers

  evDyn <- widgetHold (do {return [];}) (f <$> fgtDataEv)

  let evClick = switchPromptlyDyn (leftmost <$> evDyn)

  ev <- getPostBuild
  return $ enc $ leftmost [editFGTEv]



renderEditWidget fullHost layers = do
  rec
    let eventMessage = enc $ leftmost [ev1,ev2]

    ws <- webSocket ("ws://" <> fullHost <> "/edit/foreground/" <> idTxt) $
      def & webSocketConfig_send .~ eventMessage


    -- Controls
    ev1 <- el "tr" $ do
      let l = zip [1..] layers
      editMsgs <- forM l layerControls
      -- Select pattern and add a layer
      let miniPatternBrowser = return never
      addLayerMsg <- miniPatternBrowser

      return $ leftmost $ addLayerMsg:editMsgs

    -- Preview
    ev2 <- el "tr" $ el "td" $ do
      save <- button "Save ForeGround"
      let
        myImgUrl =
          ffor (_webSocket_recv ws)
            (\bs -> liftIO $ createObjectURL bs)
      urlEv <- performEvent myImgUrl
      urlDyn <- holdDyn "dummy" urlEv
      let dynAttr = ffor urlDyn (\u -> ("src" =: u))
      el "div" $ elDynAttr "img" dynAttr $ return ()

      return (SaveFG <$ save)

  return ()

layerControls (layerId, (patternName, fgParam)) = do
  el "td" $
    inputs <- el "table" $ do
      rec
        let
          updateResetEv = save

        s <- rangeInputWidgetWithTextEditAndReset
          "Scale:" (scaling fgParam)
            (0.1, 2.0, 0.05) updateResetEv

        c <- rangeInputWidgetWithTextEditAndReset
          "Count:" (fromIntegral (patternCount fgParam))
            (2, 128, 1) updateResetEv

        ro <- rangeInputWidgetWithTextEditAndReset
          "Rotation:" (rotationOffset fgParam)
            (-180, 180, 1) updateResetEv

        ra <- rangeInputWidgetWithTextEditAndReset
          "Radius:" (radiusOffset fgParam)
            (1, 200, 1) updateResetEv

      delete <- button "Delete Layer"
      return $ leftmost [getEditMessage layerId (s,c,ro,ra)
               , DeleteLayer layerId <$ delete]

getEditMessage :: (Reflex t)
  => LayerId
  -> (RangeInput t, RangeInput t, RangeInput t, RangeInput t)
  -> Event t [ByteString]
getEditMessage layerId (scale, count, rotate, radius) =
  Edit layerId <$> anyEditEvent
  where
    anyEditEvent = leftmost [ev1, ev2, ev3, ev4]

    ev1 = attachPromptlyDynWith f params sEv
      where f p x = p { scaling = x}

    ev2 = attachPromptlyDynWith f params cEv
      where f p x = p { patternCount = x}

    ev3 = attachPromptlyDynWith f params roEv
      where f p x = p { rotationOffset = x}

    ev4 = attachPromptlyDynWith f params raEv
      where f p x = p { radiusOffset = x}

    sEv  = ftod $ updated (_rangeInput_value scale)
    cEv  = ceiling <$> updated (_rangeInput_value count)
    roEv = ftod $ updated (_rangeInput_value rotate)
    raEv = ftod $ updated (_rangeInput_value radius)

    params = ForeGroundParams
      <$> (ceiling <$> _rangeInput_value count)
      <*> ftod (_rangeInput_value rotate)
      <*> ftod (_rangeInput_value scale)
      <*> ftod (_rangeInput_value radius)

    ftod f = fmap realToFrac f
