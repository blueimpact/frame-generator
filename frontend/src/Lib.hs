{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}

module Lib where

import Common
import Data.Aeson

import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding as E
import Data.FileEmbed
import Data.Monoid

import Reflex
import Reflex.Dom
import Control.Monad.IO.Class
import Data.Map (Map)
import qualified Data.Map as Map
-- import qualified Data.ByteString.Base64.URL

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL

import qualified GHCJS.Types    as JS
import qualified GHCJS.DOM.Types as JS
import qualified GHCJS.Foreign as JS
import qualified GHCJS.Marshal.Pure as JS (pFromJSVal)

import qualified GHCJS.DOM.Blob as Blob
import qualified JavaScript.Array as JA

foreign import javascript unsafe "window['URL']['createObjectURL']($1)" createObjectURL_ :: Blob.Blob -> IO JS.JSVal


createObjectURL :: ByteString -> IO Text
createObjectURL bs = do
  let
    run f payload = BS.useAsCString payload $ \cStr -> do
      -- Defined in Reflex.Dom
      --foreign import javascript unsafe "new Uint8Array($1_1.buf, $1_2, $2)" extractByteArray :: Ptr CChar -> Int -> IO JS.JSVal
      ba <- extractByteArray cStr $ BS.length payload
      let opt :: Maybe JS.BlobPropertyBag
          opt = Nothing
      b <- Blob.newBlob' [ba] opt
      f b
  url <- run createObjectURL_ bs
  return $ T.pack $ JS.fromJSString $ JS.pFromJSVal url

main = mainWidgetWithCss  $(embedFile "src/style.css") editForegroundWidget

editForegroundWidget ::
  (MonadWidget t m
  , PerformEvent t m) => m ()
editForegroundWidget = do
  el "div" $ text "Enter the Image ID"

  rec t <- textInput $ def & setValue .~ fmap (const "") newMessage
      b <- button "Send"
      let
          newMessage = tagDyn (_textInput_value t) $ leftmost [b, keypress Enter t]

          evMap = ffor newMessage (\t -> Map.singleton (0 ::Int) (Just t))

  listHoldWithKey Map.empty evMap createEditWidget
  return ()

--createEditWidget :: Int -> Text -> _
createEditWidget _ idTxt = do

  el "div" $ text ("Editing ForeGroundID: " <> idTxt)

  let scaleConf = RangeInputConfig 1.0 never (constDyn $ ("min" =: "0.1") <> ("max" =: "2.0") <> ("step" =: "0.05"))
  scaleRange <- rangeInput scaleConf

  let eventMessage = getEventMessage scaleRange
  ws <- webSocket ("ws://localhost:3000/edit/foreground/" <> idTxt) $ def & webSocketConfig_send .~ eventMessage

  let
    myImgUrl =
      ffor (_webSocket_recv ws)
        (\bs -> liftIO $ createObjectURL bs)

  urlEv <- performEvent myImgUrl
  urlDyn <- holdDyn "dummy" urlEv
  let dynAttr = ffor urlDyn (\u -> ("src" =: u))
  el "div" $ elDynAttr "img" dynAttr $ return ()

  return ()

getEventMessage :: (Reflex t) => RangeInput t -> Event t [ByteString]
getEventMessage scale = tagDyn message anyEvent
  where
    anyEvent = leftmost $ fmap _rangeInput_input [scale]
    message = (:[]) <$> BSL.toStrict <$> encode <$> (ClientReqEditFG
      <$> pure 8
      <*> pure 0.0
      <*>  ftod (_rangeInput_value scale)
      <*> pure 0.0)

    ftod f = fmap realToFrac f
