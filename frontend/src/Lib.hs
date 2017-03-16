{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}

module Lib where


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
      let newMessage = tag (current $ value t) $ leftmost [b, keypress Enter t]

          evMap = ffor newMessage (\t -> Map.singleton (0 ::Int) (Just t))

  listHoldWithKey Map.empty evMap createEditWidget
  return ()

createEditWidget _ idTxt = do

  el "div" $ text "Editing ForeGroundID: " <> idTxt

  rec t <- textInput $ def & setValue .~ fmap (const "") newMessage
      b <- button "Send Command"
      let newMessage = fmap (:[]) $ tag (current $ value t) $ leftmost [b, keypress Enter t]

  ws <- webSocket ("ws://localhost:3000/edit/foreground/" <> idTxt) $ def & webSocketConfig_send .~ newMessage


  let
    myImgUrl =
      ffor (_webSocket_recv ws)
        (\bs -> liftIO $ createObjectURL bs)

  urlEv <- performEvent myImgUrl
  urlDyn <- holdDyn "" urlEv
  let dynAttr = ffor urlDyn (\u -> ("src" =: u))
  elDynAttr "img" dynAttr $ return ()

  return ()
