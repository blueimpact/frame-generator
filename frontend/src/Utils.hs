-- {-# LANGUAGE TemplateHaskell #-}
-- {-# LANGUAGE RecursiveDo #-}
-- {-# LANGUAGE OverloadedStrings #-}
-- {-# LANGUAGE PartialTypeSignatures #-}
-- {-# LANGUAGE FlexibleContexts #-}
-- {-# LANGUAGE GADTs #-}

module Utils
  (createObjectURL, enc)
  where

import Reflex.Dom
import Data.Text (Text)
import qualified Data.Text as T

import qualified GHCJS.Types    as JS
import qualified GHCJS.DOM.Types as JS
import qualified GHCJS.Foreign as JS
import qualified GHCJS.Marshal.Pure as JS (pFromJSVal)

import qualified GHCJS.DOM.Blob as Blob
import qualified JavaScript.Array as JA

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL

import Data.Aeson

foreign import javascript unsafe "window['URL']['createObjectURL']($1)" createObjectURL_ :: Blob.Blob -> IO JS.JSVal

enc :: (ToJSON a, Functor f) => f a -> f [ByteString]
enc mes = (:[]) <$> BSL.toStrict <$> encode <$> mes

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

buttonE txt c = do
  ev <- button txt
  return (c <$ ev)

img url = elAttr "img"
  ("src" =: url) blank
