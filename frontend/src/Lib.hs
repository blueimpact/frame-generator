{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE FlexibleContexts #-}

module Lib where


import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding as E
import Data.FileEmbed

import Reflex
import Reflex.Dom
import Control.Monad.IO.Class
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
  (MonadWidget t m) => m ()
editForegroundWidget = do
  -- rec t <- textInput $ def & setValue .~ fmap (const "") newMessage
  --     b <- button "Send"
  --     let newMessage = fmap ((:[]) . encodeUtf8) $ tag (current $ value t) $ leftmost [b, keypress Enter t]
  el "div" $ text "Hello"
  -- ws <- webSocket "ws://localhost:3000/edit/foreground/9034544564612548176" $ def -- & webSocketConfig_send .~ newMessage

  let someBS = "hello"
  imgEv <- liftIO $ createObjectURL someBS

  el "div" $ text imgEv


  -- receivedMessages <- foldDyn (\m ms -> ms ++ [m]) [] $ _webSocket_recv ws
  -- el "p" $ text "Responses from the yesod server:"
  -- let fun bs = E.decodeUtf8 (Data.ByteString.Base64.URL.encode bs)
  --     dynAttr m =  ffor m (\bs -> ("src" =: (fun bs))
  -- _ <- el "ul" $ simpleList receivedMessages $ \m -> elDynAttr "src"  $ return ()
  return ()
