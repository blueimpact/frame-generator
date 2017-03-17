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

import Data.IORef
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
  fgRef <- liftIO $ do
    fg <- readMVar (foreGround fgd)
    newIORef $ fg
  

  sourceWS $$ Data.Conduit.List.mapMaybeM 
      (handleRequest fgRef)
    =$= sinkWSBinary
  where
    handleRequest :: (MonadIO m, MonadLogger m)
      => IORef ForeGround
      -> BS.ByteString 
      -> m (Maybe BS.ByteString)
    handleRequest fgRef req' = do
      case decodeStrict' req' of
        Just (ClientReqSaveFG) -> do
          liftIO $ do
            fgVal <- readIORef fgRef 
            takeMVar (foreGround fgd) -- discard old
            putMVar (foreGround fgd) fgVal

          return Nothing

        Just req@(ClientReqEditFG _ _ _ _) -> do
          $logInfo $ "edit foreground: Valid request"
          fg <- liftIO $ readMVar (foreGround fgd)
          let fgparam = (foreGroundParams fg)
                { scaling = clientReqEditFGScaling req
                , patternCount = clientReqEditFGCount req
                , rotationOffset = clientReqEditFGRotation req
                , radiusOffset = clientReqEditFGRadiusOff req}
              newFG = getForeGround fgparam
                        (pattern fgd)

              pngData = encodeToPng newFG previewSize

          liftIO $ writeIORef fgRef (ForeGround newFG fgparam 
            (foreGroundPng fg))
          return $ Just pngData

        _ -> return Nothing

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

getEditMaskR :: ForeGroundID -> Handler Html
getEditMaskR fgID = do
  $logInfo $ "Doing edit Mask"
  appSt <- getYesod

  db <- liftIO $ readMVar (foreGroundDB appSt)

  case Map.lookup fgID db of
    Nothing -> redirect HomeR
    Just fgd -> do
      webSockets (editMaskWebSocketWidget appSt fgd)
      fg <- liftIO $ readMVar (foreGround fgd)

      dilParam <- lookupGetParam "dilate"
      blurParam <- lookupGetParam "blur"
      let dilVal :: Int
          dilVal = getParamFromMaybe 0 dilParam

          blurVal :: Int
          blurVal = getParamFromMaybe 0 blurParam

      let (dil,ff,subt,msk) =
            getMask (foreGroundDia fg) previewSize maskParams
          maskParams = MaskParams dilVal blurVal

      pngID <- liftIO $ do
        addToMVarMap (pngDB appSt) PngID dil

      pngID2 <- liftIO $ do
        addToMVarMap (pngDB appSt) PngID ff

      pngID3 <- liftIO $ do
        addToMVarMap (pngDB appSt) PngID subt

      liftIO $ do
        tryTakeMVar (mask fgd) -- discard old
        putMVar (mask fgd) (Mask maskParams msk)

      defaultLayout [whamlet|$newline never
          <p>
          <a href=@{EditMaskR fgID}>Edit Mask
          <img src=@{PngR pngID}>
          <img src=@{PngR pngID2}>
          <img src=@{PngR pngID3}>
|]

editMaskWebSocketWidget appSt fgd = do
  $logInfo $ "edit mask: Websocket version"
  
  maskRef <- liftIO $ do
    fg <- readMVar (foreGround fgd)

    let (_,_,_,msk) = 
          getMask (foreGroundDia fg)
            previewSize (MaskParams 2 2)
    newIORef $
      Mask (MaskParams 2 2) msk

  sourceWS $$ Data.Conduit.List.mapMaybeM 
      (handleRequest maskRef)
    =$= sinkWSBinary
  where
    handleRequest :: (MonadIO m, MonadLogger m)
      => IORef Mask
      -> BS.ByteString
      -> m (Maybe BS.ByteString)
    handleRequest maskRef req' = do
      case decodeStrict' req' of
        Just (ClientReqSaveMask) -> do
          liftIO $ do
            msk <- readIORef maskRef
            tryTakeMVar (mask fgd) -- discard old
            putMVar (mask fgd) msk
          return Nothing

        Just req@(ClientReqEditMask _ _) -> do
          $logInfo $ "edit mask: Valid request"
          -- Avoid this readMVar
          fg <- liftIO $ readMVar (foreGround fgd)

          let maskParams = MaskParams 
                (clientReqEditMaskDilate req)
                (clientReqEditMaskBlur req)

              (_,_,subt,msk) = 
                getMask (foreGroundDia fg)
                  previewSize maskParams

          liftIO $ writeIORef maskRef (Mask maskParams msk)
          return $ Just subt

        _ -> return Nothing

