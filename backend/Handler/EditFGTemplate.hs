{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell   #-}
module Handler.EditFGTemplate where

import Import
import Yesod.WebSockets
import Utils.FrameCreator
import Utils.PatternManage
import Utils.Misc
import Common
import Message

import qualified Data.Map as Map
import Control.Monad
import Data.Conduit
import qualified Data.Conduit.List
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NE
import Diagrams.Prelude (Diagram)
import Diagrams.Backend.Rasterific (Rasterific)
import Data.Aeson
import qualified Control.Lens

getEditForeGroundR :: ForeGroundTemplateDBId -> Handler Html
getEditForeGroundR fgtID = do

  fgt <- runDB $ get fgtID
  let fgtData = join $ decodeStrict <$>
                foreGroundTemplateDBData <$> fgt
      pats = (map fst) <$> fgtData
  dias <- getPatternsDia <$> pats
  webSockets <$> (webSocketServer fgtID <$> fgtData <$> dias )
  redirect HomeR

webSocketServer fgtID fgtData' dias' = do
  $logInfo $ "Doing edit foreground tempate"

  fgtDataRef <- newIORef fgtData'
  diasRef <- newIORef dias'
  imgDataRef <- newIORef ""

  sourceWS $$ Data.Conduit.List.mapMaybeM
    (handleRequest fgtDataRef diasRef)
    =$= sinkWSBinary
  where
    handleRequest :: (MonadIO m, MonadLogger m)
      => IORef (ForeGroundTemplateData)
      -> IORef (NonEmpty (Diagram Rasterific))
      -> IORef (BSL.ByteString)
      -> BSL.ByteString
      -> m (Maybe BSL.ByteString)
    handleRequest fgtDataRef req' imgData = do
      case decode req' of
        Just (Edit layerId params) -> do
          d <- readIORef fgtDataRef
          dias <- readIORef diasRef
          -- Better method?
          let newD = Control.Lens.imap
                (\i a@(p,_) -> if i == layerId
                  then (p,params)
                  else a) d

              resDia = getForeGround dias (NE.map snd newD)
              resImg = encodeToPngLazy resDia 600

          writeIORef imgDataRef resImg
          writeIORef fgtDataRef newD
          return (Just resImg)

        Just (AddLayer pat) -> do
          d <- readIORef fgtDataRef
          dias <- readIORef diasRef

          dia <- getPatternsDia pat
          let newD = appendNE d (pat, defFGParams)
              defFGParams = ForeGroundParams 8 0 1.0 100

              newDias = appendNE dias <$> dia

              resDia = getForeGround <$> newDias
                <*> pure (NE.map snd newD)
              resImg = encodeToPngLazy <$> resDia <*> pure 600

          forM resImg $ const $ do
            writeIORef diasRef
            writeIORef fgtDataRef

          forM resImg (writeIORef imgDataRef)
          return resImg

        Just (DeleteLayer layerId) -> do
          d <- readIORef fgtDataRef
          dias <- readIORef diasRef

          let newD = NE.fromList $
                (take (layerId - 1) d) ++ (drop layerId d)

              newDias = NE.fromList $
                (take (layerId - 1) dias) ++ (drop layerId dias)

              resDia = getForeGround newDias (NE.map snd newD)
              resImg = encodeToPngLazy resDia pure 600

          writeIORef diasRef
          writeIORef fgtDataRef

          return $ Just resImg

        Just SaveFG -> do
          d <- readIORef fgtDataRef
          imgData <- readIORef images
          runDB $ update fgtID
            [ForeGroundTemplateData =. (BSL.toStrict.encode d)]
          savePng (Just (fgtemplatesDir, tshow fgtID)) $
            BSL.toStrict imgDataRef
          return Nothing

        _ -> return Nothing

-- editMaskWebSocketWidget appSt fgd = do
--   $logInfo $ "edit mask: Websocket version"

--   maskRef <- liftIO $ do
--     fg <- readMVar (foreGround fgd)

--     let (_,_,_,msk) =
--           getMask (foreGroundDia fg)
--             previewSize (MaskParams 2 2)
--     newIORef $
--       Mask (MaskParams 2 2) msk

--   sourceWS $$ Data.Conduit.List.mapMaybeM
--       (handleRequest maskRef)
--     =$= sinkWSBinary
--   where
--     handleRequest :: (MonadIO m, MonadLogger m)
--       => IORef Mask
--       -> BS.ByteString
--       -> m (Maybe BS.ByteString)
--     handleRequest maskRef req' = do
--       case decodeStrict' req' of
--         Just (ClientReqSaveMask) -> do
--           liftIO $ do
--             msk <- readIORef maskRef
--             tryTakeMVar (AppData.mask fgd) -- discard old
--             putMVar (AppData.mask fgd) msk
--           return Nothing

--         Just (ClientReqEditMask maskParams) -> do
--           $logInfo $ "edit mask: Valid request"
--           fg <- liftIO $ readMVar (foreGround fgd)

--           let
--               (_,_,subt,msk) =
--                 getMask (foreGroundDia fg)
--                   previewSize maskParams

--           liftIO $ writeIORef maskRef (Mask maskParams msk)
--           return $ Just subt

--         _ -> return Nothing
