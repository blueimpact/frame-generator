{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell   #-}
module EditFrame where

import Foundation
import Yesod.Core
import Yesod.WebSockets
import FrameCreator
import Utils
import Common

import Control.Concurrent.MVar
import qualified Data.Map as Map
import Control.Monad
import Data.Conduit
import qualified Data.Conduit.List
import qualified Data.ByteString as BS
import Data.Aeson

getEditForeGroundR :: ForeGroundID -> Handler Html
getEditForeGroundR fgID = do
  $logInfo $ "Doing edit foreground"
  appSt <- getYesod

  db <- liftIO $ readMVar (foreGroundDB appSt)

  case Map.lookup fgID db of
    Nothing -> redirect HomeR
    Just fgd -> do
      webSockets (editForeGroundWebSocketWidget appSt fgd)
      $logInfo $ "edit foreground: HTTP version"

      fg <- liftIO $ takeMVar (foreGround fgd)

      newParams <- getNewForeGroundParams (foreGroundParams fg)

      let
        newFgDia = getForeGround newParams (pattern fgd)

        pngData = encodeToPng newFgDia previewSize

      pngID <- liftIO $ do
        addToMVarMap (pngDB appSt) PngID pngData

      let newFg = ForeGround newFgDia newParams pngID

      liftIO $ putMVar (foreGround fgd) newFg

      defaultLayout [whamlet|$newline never
          <p>
          <img src=@{PngR pngID}>
          <a href=@{EditForeGroundR fgID}>Edit Foreground
          <a href=@{CreateMaskR fgID}>Create Mask
|]

-- Do editing in interactive manner
-- editForeGroundWebSocketWidget ::
editForeGroundWebSocketWidget appSt fgd = do
  $logInfo $ "edit foreground: Websocket version"
  sourceWS $$ Data.Conduit.List.mapMaybeM handleRequest
    =$= sinkWSBinary
  where
    handleRequest :: (MonadIO m, MonadLogger m) => BS.ByteString -> m (Maybe BS.ByteString)
    handleRequest req' = do
      case decodeStrict' req' of
        Nothing -> return Nothing
        Just (ClientReqEditFG c r s _) -> do
          $logInfo $ "edit foreground: Valid request"
          fg <- liftIO $ readMVar (foreGround fgd)
          let fgparam = (foreGroundParams fg)
                {scaling = s}
              newFG = getForeGround fgparam
                        (pattern fgd)

              pngData = encodeToPng newFG previewSize

          return $ Just pngData

-- Fetch the parameters by URL/RESTful way
getNewForeGroundParams :: ForeGroundParams -> Handler ForeGroundParams
getNewForeGroundParams fgParams = do
  patCount <- lookupGetParam "count"
  rotOff <- lookupGetParam "rotation"
  scale <- lookupGetParam "scaling"
  radOff <- lookupGetParam "radoffset"

  let
    newFgParams = ForeGroundParams
      (getParamFromMaybe (patternCount   fgParams) patCount)
      (radius         fgParams)
      (getParamFromMaybe (rotationOffset fgParams) rotOff)
      (getParamFromMaybe (scaling        fgParams) scale)
      (getParamFromMaybe (radiusOffset   fgParams) radOff)
      (template       fgParams)
  return newFgParams
