{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell   #-}
module Handler.EditFrame where

import Import
import Yesod.WebSockets
import Utils.FrameCreator
import Utils.Misc
import Common
import AppData

import qualified Data.Map as Map
import Control.Monad
import Data.Conduit
import qualified Data.Conduit.List
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import Data.Aeson

getEditForeGroundR :: ForeGroundID -> Handler Html
getEditForeGroundR fgID = do
  $logInfo $ "Doing edit foreground"
  appSt <- appData <$> getYesod

  db <- liftIO $ readMVar (foreGroundDB appSt)

  case Map.lookup fgID db of
    Nothing -> redirect HomeR
    Just fgd -> do
      webSockets (editForeGroundWebSocketWidget appSt fgd)
      $logInfo $ "edit foreground: HTTP version"

      fg <- liftIO $ takeMVar (foreGround fgd)

      newParams <- getNewForeGroundParams (foreGroundParams fg)

      let
        newFgDia = getForeGround (patternData fgd)
          newParams

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

            db <- readMVar (pngDB appSt)

            let pngData = encodeToPng
                  (foreGroundDia fgVal) previewSize

                newDB = Map.update (const $ Just pngData)
                        (foreGroundPng fgVal) db

            swapMVar (pngDB appSt) newDB
            takeMVar (foreGround fgd) -- discard old
            putMVar (foreGround fgd) fgVal

          return Nothing

        Just (ClientReqEditFG fgparam) -> do
          $logInfo $ "edit foreground: Valid request"
          fg <- liftIO $ readMVar (foreGround fgd)
          let 
              newFG = getForeGround 
                        (patternData fgd)
                        fgparam

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
      (getParamFromMaybe (rotationOffset fgParams) rotOff)
      (getParamFromMaybe (scaling        fgParams) scale)
      (getParamFromMaybe (radiusOffset   fgParams) radOff)
  return newFgParams

getEditMaskR :: ForeGroundID -> Handler Html
getEditMaskR fgID = do
  $logInfo $ "Doing edit Mask"
  appSt <- appData <$> getYesod

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
        tryTakeMVar (AppData.mask fgd) -- discard old
        putMVar (AppData.mask fgd) (Mask maskParams msk)

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
            tryTakeMVar (AppData.mask fgd) -- discard old
            putMVar (AppData.mask fgd) msk
          return Nothing

        Just (ClientReqEditMask maskParams) -> do
          $logInfo $ "edit mask: Valid request"
          fg <- liftIO $ readMVar (foreGround fgd)

          let
              (_,_,subt,msk) =
                getMask (foreGroundDia fg)
                  previewSize maskParams

          liftIO $ writeIORef maskRef (Mask maskParams msk)
          return $ Just subt

        _ -> return Nothing

-- Serve data to edit pane via websocket
getEditPaneR :: ForeGroundID -> Handler Html
getEditPaneR fgID = do
  appSt <- appData <$> getYesod

  db <- liftIO $ readMVar (foreGroundDB appSt)

  case Map.lookup fgID db of
    Nothing -> redirect HomeR
    Just fgd -> do
      webSockets (editPaneWebSocket appSt fgd)
      redirect HomeR

editPaneWebSocket appSt fgd = do
  $logInfo $ "edit pane: Websocket version"

  sourceWS $$ Data.Conduit.List.mapMaybeM
      (handleRequest )
    =$= sinkWSBinary
  where
    handleRequest :: (MonadIO m, MonadLogger m)
      => BSL.ByteString
      -> m (Maybe BSL.ByteString)
    handleRequest req' = do
      case decode' req' of
        Just (GetFGDefaultParams) -> do
          fg <- liftIO $ readMVar (foreGround fgd)
          return $ Just $ encode $ foreGroundParams fg
        Just (GetMaskDefaultParams) -> do
          msk <- liftIO $ readMVar (AppData.mask fgd)
          return $ Just $ encode $ maskParams msk
        _ -> return Nothing
