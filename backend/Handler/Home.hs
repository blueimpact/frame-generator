{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell   #-}
module Handler.Home where

import Import
import Common
import Data.Text (Text)
import qualified Data.Text as T
import System.Directory

getHomeR :: Handler Html
getHomeR = redirect (StaticR editapp_index_html)

-- Upload form
data UploadPatternForm = UploadPatternForm
  Text
  FileInfo
  (Maybe FileInfo)
  (Maybe FileInfo)
  (Maybe FileInfo)
  (Maybe FileInfo)
  (Maybe FileInfo)
  (Maybe FileInfo)
  (Maybe FileInfo)

patternForm = renderDivs $ UploadPatternForm
  <$> areq textField "Group Name" Nothing
  <*> areq fileField "Pattern File 1" Nothing
  <*> aopt fileField "Pattern File 2" Nothing
  <*> aopt fileField "Pattern File 3" Nothing
  <*> aopt fileField "Pattern File 4" Nothing
  <*> aopt fileField "Pattern File 5" Nothing
  <*> aopt fileField "Pattern File 6" Nothing
  <*> aopt fileField "Pattern File 7" Nothing
  <*> aopt fileField "Pattern File 8" Nothing

getUploadPatternR :: Handler Html
getUploadPatternR = do
    ((_, widget), enctype) <- runFormPost patternForm
    defaultLayout [whamlet|
      <form method=post enctype=#{enctype}>
        ^{widget}
        <p>
        <input type=submit>
|]

postUploadPatternR :: Handler Html
postUploadPatternR = do
  ((result, widget), enctype) <- runFormPost patternForm
  $logDebug "Trying to read uploaded pattern"

  case result of
    FormSuccess (UploadPatternForm g f1 f2 f3 f4 f5 f6 f7 f8) -> do
      let
          doFile f = do
            (fileMove f) (dir ++ "/" ++ (T.unpack (fileName f)))

          dir = T.unpack $ "./" <> patternsDir <> "/" <> g

          saveFiles = do
            createDirectoryIfMissing True dir
            doFile f1
            mapM doFile f2
            mapM doFile f3
            mapM doFile f4
            mapM doFile f5
            mapM doFile f6
            mapM doFile f7
            mapM doFile f8

      liftIO saveFiles

      redirect HomeR
    _ -> do
      $logError "Error in upload pattern"
      redirect HomeR
