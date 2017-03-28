{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NoMonomorphismRestriction #-}


module EditForeGround where

import Common
import Utils

import Data.Aeson

import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding as E
import Data.Monoid

import Reflex
import Reflex.Dom
import Reflex.Dom.Contrib.Widgets.EditInPlace
import Control.Monad.IO.Class
import Data.Map (Map)
import qualified Data.Map as Map

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS

editForegroundWidget = createEditWidget

rangeInputWidgetWithTextEditAndReset ::
  (MonadWidget t m)
  => Text                   -- Label
  -> Double                 -- Initial value
  -> (Float, Float, Float)  --
  -> Event t ()             -- Update reset value, when saving
  -> m (RangeInput t)

rangeInputWidgetWithTextEditAndReset
  label initVal' (min, max, step) resetUpd = do

  rec
    let
        initVal = realToFrac initVal'
        val = tshow <$> _rangeInput_value ri

        setValEv1 = read <$> (T.unpack <$> e)
        setValEv2 = tagPromptlyDyn resetValDyn r

        setValEv = leftmost [setValEv1, setValEv2]

    resetValDyn <- holdDyn initVal
      (tagPromptlyDyn (_rangeInput_value ri) resetUpd)

    (e,r) <- el "tr" $ do
      el "table" $ do
        el "td" $ text label
        evValChange <- el "td" $
          editInPlace (constant True) val
        resetEv <- el "td" $
          button "Reset"
        return (evValChange, resetEv)

    ri <- el "tr" $ rangeInput $
      RangeInputConfig initVal setValEv
        (constDyn $ ("min" =: tshow min) <> ("max" =: tshow max)
          <> ("step" =: tshow step))

  return ri

tshow :: (Show a) => a -> Text
tshow v = T.pack $ show v

createEditWidget url idTxt fgParam = do

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

      save <- button "Save ForeGround"
    return (s,c,ro,ra,save)

  let eventMessage = getEventMessage inputs

  ws <- webSocket ("ws://" <> url <> "/edit/foreground/" <> idTxt) $ def & webSocketConfig_send .~ eventMessage

  let
    myImgUrl =
      ffor (_webSocket_recv ws)
        (\bs -> liftIO $ createObjectURL bs)

  urlEv <- performEvent myImgUrl
  urlDyn <- holdDyn "dummy" urlEv
  let dynAttr = ffor urlDyn (\u -> ("src" =: u))
  el "div" $ elDynAttr "img" dynAttr $ return ()

  return ()

getEventMessage :: (Reflex t) =>
     (RangeInput t, RangeInput t, RangeInput t, RangeInput t
      , Event t ())
  -> Event t [ByteString]
getEventMessage (scale, count, rotate, radius, save) =
  leftmost $ [saveEv, editEv]
  where
    saveEv = enc $ const ClientReqSaveFG <$> save
    editEv = enc $ ClientReqEditFG <$> anyEditEvent

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
