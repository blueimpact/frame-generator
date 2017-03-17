{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}

module EditForeGround where

import Common
import Utils

import Data.Aeson

import Data.Text (Text)
import Data.Text.Encoding as E
import Data.Monoid

import Reflex
import Reflex.Dom
import Control.Monad.IO.Class
import Data.Map (Map)
import qualified Data.Map as Map

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL

editForegroundWidget ::
  (MonadWidget t m
  , PerformEvent t m) => m ()
editForegroundWidget = do
  el "div" $ text "Enter the Image ID"

  currentPageUrl <-textInput $ def {_textInputConfig_initialValue = "Yesod URL"}
  rec t <- textInput $ def & setValue .~ fmap (const "") newMessage
      b <- button "Send"
      let
          urlValDyn = zipDyn (_textInput_value currentPageUrl) (_textInput_value t)
          newMessage = tagDyn urlValDyn $ leftmost [b, keypress Enter t]

          evMap = ffor newMessage (\t -> Map.singleton (0 ::Int) (Just t))

  listHoldWithKey Map.empty evMap createEditWidget
  return ()

--createEditWidget :: Int -> Text -> _
createEditWidget _ (url, idTxt) = do

  el "div" $ text ("Editing ForeGroundID: " <> idTxt)

  let scaleConf =
        RangeInputConfig 1.0 never
          (constDyn $ ("min" =: "0.1") <> ("max" =: "2.0") <> ("step" =: "0.05"))

      countConf =
        RangeInputConfig 8 never
          (constDyn $ ("min" =: "2") <> ("max" =: "128") <> ("step" =: "1"))

      rotateConf =
        RangeInputConfig 0 never
          (constDyn $ ("min" =: "-180") <> ("max" =: "180") <> ("step" =: "1"))

      radOffConf =
        RangeInputConfig 100 never
          (constDyn $ ("min" =: "1") <> ("max" =: "200") <> ("step" =: "1"))

  inputs <- el "table" $ do
    el "tr" $ text "Scale"
    s <- el "tr" $ do
      rangeInput scaleConf

    el "tr" $ text "Count"
    c <- el "tr" $ do
      rangeInput countConf 

    el "tr" $ text "Rotation"
    ro <- el "tr" $ do
      rangeInput rotateConf 

    el "tr" $ text "Radius"
    ra <- el "tr" $ do
      rangeInput radOffConf

    return (s,c,ro,ra)

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
     (RangeInput t, RangeInput t, RangeInput t, RangeInput t)
  -> Event t [ByteString]
getEventMessage (scale, count, rotate, radius) = tagDyn message anyEvent
  where
    anyEvent = leftmost $ fmap _rangeInput_input [scale, count, rotate, radius]

    message = (:[]) <$> BSL.toStrict <$> encode <$> (ClientReqEditFG
      <$> (ceiling <$> _rangeInput_value count)
      <*> ftod (_rangeInput_value rotate)
      <*> ftod (_rangeInput_value scale)
      <*> ftod (_rangeInput_value radius))

    ftod f = fmap realToFrac f
