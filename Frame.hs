{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
module Frame where

import Foundation
import Yesod.Core
import FrameCreator
import Utils

import Control.Concurrent.MVar
import qualified Data.Map as Map

-- Spec
-- Create foreground for pattern with default setup
--getForeGroundR :: PatternId -> Handler Html
--getForeGroundR pId = do

getPreviewPatternR :: PatternID -> Handler Html
getPreviewPatternR patID = do
  appSt <- getYesod

  db <- liftIO $ readMVar (patternDB appSt)

  let pd = Map.lookup patID db

  case pd of
    Nothing -> redirect HomeR
    Just pat -> do
      let pngData = encodeToPng (origPatternData pat) 500

      pngID <- liftIO $ addToMVarMap (pngDB appSt) PngID pngData

      defaultLayout [whamlet|$newline never
        <p>
        <img src=@{PngR pngID}>
        <a href=@{MakeForeGroundR patID}>Make Foreground
|]

getMakeForeGroundR :: PatternID -> Handler Html
getMakeForeGroundR patID = do
  appSt <- getYesod

  db <- liftIO $ readMVar (patternDB appSt)

  case Map.lookup patID db of
    Nothing -> redirect HomeR
    Just pat -> do
      let
        fgParams =
          ForeGroundParams
            (defaultCount pat)
            (defaultRadius pat)
            0 -- radiusOffset
            1.0 -- scaling
            0 -- radiusOffset
            (origTemplate pat)

        fg = getForeGround fgParams (origPatternData pat)

        pngData = encodeToPng fg 500

      pngID <- liftIO $
        addToMVarMap (pngDB appSt) PngID pngData

      fgID <- liftIO $ do
        mvar <- newMVar
          (ForeGround fg fgParams pngID)

        addToMVarMap (foreGroundDB appSt) ForeGroundID
          (ForeGroundData (origPatternData pat) mvar)

      defaultLayout [whamlet|$newline never
          <p>
          <img src=@{PngR pngID}>
          <a href=@{EditForeGroundR fgID}>Edit Foreground
          <a href=@{ForeGroundMaskR fgID}>Foreground Mask
|]

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

getEditForeGroundR :: ForeGroundID -> Handler Html
getEditForeGroundR fgID = do
  appSt <- getYesod

  db <- liftIO $ readMVar (foreGroundDB appSt)

  case Map.lookup fgID db of
    Nothing -> redirect HomeR
    Just fgd -> do
      fg <- liftIO $ takeMVar (foreGround fgd)

      newParams <- getNewForeGroundParams (foreGroundParams fg)

      let
        newFgDia = getForeGround newParams (pattern fgd)

        pngData = encodeToPng newFgDia 500

      pngID <- liftIO $ do
        addToMVarMap (pngDB appSt) PngID pngData

      let newFg = ForeGround newFgDia newParams pngID

      liftIO $ putMVar (foreGround fgd) newFg

      defaultLayout [whamlet|$newline never
          <p>
          <img src=@{PngR pngID}>
          <a href=@{EditForeGroundR fgID}>Edit Foreground
          <a href=@{ForeGroundMaskR fgID}>Foreground Mask
|]

getForeGroundMaskR :: ForeGroundID -> Handler Html
getForeGroundMaskR fgID = do
  appSt <- getYesod

  db <- liftIO $ readMVar (foreGroundDB appSt)

  case Map.lookup fgID db of
    Nothing -> redirect HomeR
    Just fgd -> do
      fg <- liftIO $ readMVar (foreGround fgd)

      dilParam <- lookupGetParam "dilate"
      blurParam <- lookupGetParam "blur"
      let dilVal :: Int
          dilVal = getParamFromMaybe 0 dilParam

          blurVal :: Int
          blurVal = getParamFromMaybe 0 blurParam

          (dil,ff,subt) = getMask (foreGroundDia fg) 800 dilVal blurVal

      pngID <- liftIO $ do
        addToMVarMap (pngDB appSt) PngID dil

      pngID2 <- liftIO $ do
        addToMVarMap (pngDB appSt) PngID ff

      pngID3 <- liftIO $ do
        addToMVarMap (pngDB appSt) PngID subt

      defaultLayout [whamlet|$newline never
          <p>
          <img src=@{PngR pngID}>
          <img src=@{PngR pngID2}>
          <img src=@{PngR pngID3}>
          <a href=@{EditForeGroundR fgID}>Edit Foreground
|]

getPngR :: PngID -> Handler TypedContent
getPngR pngID = do
  appSt <- getYesod
  db <- liftIO $ readMVar (pngDB appSt)

  let pngData = Map.lookup pngID db
  case pngData of
    Nothing -> notFound
    Just d -> respondSource "image/png" (sendChunkBS d)

