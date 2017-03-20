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
        <a href=@{MakeForeGroundR patID}>Make Foreground
        <img src=@{PngR pngID}>
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
        <p>Select a frame to apply
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
            0 -- rotationOffset
            1.0 -- scaling
            100 -- radiusOffset %
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

          -- <a href=@{EditForeGroundR fgID}>Edit Foreground
          -- <a href=@{CreateMaskR fgID}> Foreground Mask
      defaultLayout [whamlet|
          <p>ForeGroundID = #{show (unForeGroundID fgID)}
          <p>Use this ID on the edit page
          <p><img src=@{PngR pngID}>
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
          <a href=@{EditMaskR fgID}>Edit Mask
          <img src=@{PngR pngID}>
          <img src=@{PngR pngID2}>
          <img src=@{PngR pngID3}>
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
      sizeP <- lookupGetParam "size"
      let size = getParamFromMaybe previewSize sizeP

      fg <- liftIO $ readMVar (foreGround fgd)
      m <- liftIO $ readMVar (mask fgd)
      let
        maskedImgData =
          createFrame img fg m size

      pngID <- liftIO $ do
        addToMVarMap (pngDB appSt) PngID maskedImgData

      let
      defaultLayout [whamlet|$newline never
          <p>Get High Quality

            <a href=@?{(CreateFrameR fgID imgID, [("size","800")])}>800,
            <a href=@?{(CreateFrameR fgID imgID, [("size","1200")])}>1200
          <p>If high quality images have improper mask, then increase the blur/dilute amount
          <p>
            <img src=@{PngR pngID}>
|]
