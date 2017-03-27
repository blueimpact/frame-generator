{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell   #-}
module Handler.Home where

import Import
import AppData

import qualified Data.Map as Map
import System.Random
import qualified Data.Text as T
import Data.Conduit.List

import Utils.FrameCreator
import Utils.Misc

-- Spec
-- Upload the user given pattern and do error checking
-- If pattern is ok then give a link to the pattern id
-- Store the pattern in memory App

data UploadPatternForm = UploadPatternForm
  FileInfo ForeGroundTemplate

patternForm = renderDivs $ UploadPatternForm
  <$> areq fileField "Pattern File" Nothing
  <*> areq (radioField optionsEnum) "Template"
        (Just Horizontal)

getHomeR :: Handler Html
getHomeR = do
    defaultLayout [whamlet|
      <p>Welcome!
      <p>
        <a href=@{UploadPatternR}>Upload Pattern
        Images with only alpha layers tested!

      <p>
        <a href=@{UploadBackgroundImageR}>Upload Image
        This can be anything, it will be cropped as a square left most side.
|]

getUploadPatternR :: Handler Html
getUploadPatternR = do
    ((_, widget), enctype) <- runFormPost patternForm
    defaultLayout [whamlet|$newline never
      <form method=post enctype=#{enctype}>
        ^{widget}
        <p>
        <input type=submit>Submit
|]

postUploadPatternR :: Handler Html
postUploadPatternR = do
  ((result, widget), enctype) <- runFormPost patternForm
  $logDebug "Trying to read uploaded pattern"

  fd <- case result of
    FormSuccess (UploadPatternForm f t) -> do
      -- Get all file data
      d <- runConduit
        ((fileSource f) =$= (Data.Conduit.List.fold (<>) ""))

      return $ Just (d,t)
    _ -> return Nothing

  let patData = join $ parsePatternData <$> fd

  case fd of
    Nothing -> $logError $ "Could not read input pattern"
    Just _ -> return ()

  case patData of
    Nothing -> do
      $logError $ "Could not parse pattern"
      redirect HomeR
      -- Set message and return to home

    Just pd -> do -- Store pd and go to preview
      $logInfo $ "Parse succesful"

      appSt <- appData <$> getYesod

      patID <- liftIO $ addToMVarMap (patternDB appSt) PatternID pd

      redirect $ MakeForeGroundR patID

data UploadBackgroundImageForm = UploadBackgroundImageForm FileInfo

bgImageForm = renderDivs $ UploadBackgroundImageForm
  <$> areq fileField "Background Image" Nothing

getUploadBackgroundImageR :: Handler Html
getUploadBackgroundImageR = do
    ((_, widget), enctype) <- runFormPost bgImageForm
    defaultLayout [whamlet|$newline never
      <form method=post enctype=#{enctype}>
        ^{widget}
        <p>
        <input type=submit>Submit
|]

postUploadBackgroundImageR :: Handler Html
postUploadBackgroundImageR = do
  ((result, widget), enctype) <- runFormPost bgImageForm
  $logDebug "Trying to read uploaded image"

  fd <- case result of
    FormSuccess (UploadBackgroundImageForm f) -> do
      -- Get all file data
      d <- runConduit
        ((fileSource f) =$= (Data.Conduit.List.fold (<>) ""))

      return $ Just d
    _ -> return Nothing

  let imgData = join $ parseBackgroundImageData <$> fd

  case fd of
    Nothing -> $logError $ "Could not read input image"
    Just _ -> return ()

  case imgData of
    Nothing -> do
      $logError $ "Could not parse file"
      redirect HomeR
      -- Set message and return to home

    Just img -> do -- Store pd and go to preview
      $logInfo $ "Parse succesful"

      appSt <- appData <$> getYesod

      imgID <- liftIO $ addToMVarMap (imageDB appSt)
       BackgroundImageID img
      redirect $ PreviewBackgroundImageR imgID

getPatternsListR :: Handler Html
getPatternsListR = getHomeR

getBackgroundImageListR :: Handler Html
getBackgroundImageListR = getHomeR

postEditMaskR :: ForeGroundID -> Handler Html
postEditMaskR _ = undefined
