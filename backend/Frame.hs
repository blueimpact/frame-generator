{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
module Frame where

import Foundation
import Yesod.Core
import FrameCreator
import Utils

import Control.Concurrent.MVar
import qualified Data.Map as Map
import Control.Monad

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
      let pngData = encodeToPng (origPatternData pat) previewSize

      pngID <- liftIO $ addToMVarMap (pngDB appSt) PngID pngData

      defaultLayout [whamlet|$newline never
        <p>
        <img src=@{PngR pngID}>
        <a href=@{MakeForeGroundR patID}>Make Foreground
|]

getPreviewBackgroundImageR :: BackgroundImageID -> Handler Html
getPreviewBackgroundImageR imgID = do
  appSt <- getYesod

  db <- liftIO $ readMVar (imageDB appSt)

  case Map.lookup imgID db of
    Nothing -> redirect HomeR
    Just img -> do
      let pngData = encodeToPng (origBackgroundImage img) previewSize

      pngID <- liftIO $ addToMVarMap (pngDB appSt) PngID pngData

      foreGrounds <- liftIO $ do
        fgDB <- readMVar (foreGroundDB appSt)
        forM (Map.toList fgDB)
          (\(k,fgd) -> do
            x <- readMVar (foreGround fgd)
            return (k,x))


      defaultLayout [whamlet|
        <p>
        <img src=@{PngR pngID}>
        $forall (k,fg) <- foreGrounds
          <a href=@{CreateFrameR k imgID}>
            <img src=@{PngR (foreGroundPng fg)}>
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

        pngData = encodeToPng fg previewSize

      pngID <- liftIO $
        addToMVarMap (pngDB appSt) PngID pngData

      fgID <- liftIO $ do
        mvar <- newMVar
          (ForeGround fg fgParams pngID)

        -- This will be created later when foreground is finalised
        maskMvar <- newEmptyMVar

        addToMVarMap (foreGroundDB appSt) ForeGroundID
          (ForeGroundData (origPatternData pat) mvar maskMvar)

      defaultLayout [whamlet|$newline never
          <p>
          <img src=@{PngR pngID}>
          <a href=@{EditForeGroundR fgID}>Edit Foreground
          <a href=@{CreateMaskR fgID}>Foreground Mask
|]

-- Create a basic default mask
getCreateMaskR :: ForeGroundID -> Handler Html
getCreateMaskR fgID = do
  appSt <- getYesod

  db <- liftIO $ readMVar (foreGroundDB appSt)

  case Map.lookup fgID db of
    Nothing -> redirect HomeR
    Just fgd -> do
      fg <- liftIO $ readMVar (foreGround fgd)

      let (dil,ff,subt,msk) =
            getMask (foreGroundDia fg) previewSize maskParams
          maskParams = MaskParams 2 2

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
          <img src=@{PngR pngID}>
          <img src=@{PngR pngID2}>
          <img src=@{PngR pngID3}>
          <a href=@{EditMaskR fgID}>Edit Mask
|]


getEditMaskR :: ForeGroundID -> Handler Html
getEditMaskR fgID = do
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
          <img src=@{PngR pngID}>
          <img src=@{PngR pngID2}>
          <img src=@{PngR pngID3}>
          <a href=@{EditMaskR fgID}>Edit Mask
|]

getPngR :: PngID -> Handler TypedContent
getPngR pngID = do
  appSt <- getYesod
  db <- liftIO $ readMVar (pngDB appSt)

  let pngData = Map.lookup pngID db
  case pngData of
    Nothing -> notFound
    Just d -> respondSource "image/png" (sendChunkBS d)

getCreateFrameR :: ForeGroundID -> BackgroundImageID -> Handler Html
getCreateFrameR fgID imgID = do
  appSt <- getYesod

  fgDB <- liftIO $ readMVar (foreGroundDB appSt)
  imgDB <- liftIO $ readMVar (imageDB appSt)

  case (Map.lookup fgID fgDB, Map.lookup imgID imgDB) of
    (Nothing, _) -> redirect HomeR
    (_, Nothing) -> redirect HomeR
    (Just fgd, Just img) -> do
      fg <- liftIO $ readMVar (foreGround fgd)
      m <- liftIO $ readMVar (mask fgd)
      let
        maskedImgData =
          createFrame img fg m previewSize

      pngID <- liftIO $ do
        addToMVarMap (pngDB appSt) PngID maskedImgData

      defaultLayout [whamlet|$newline never
          <p>
          <img src=@{PngR pngID}>
|]
