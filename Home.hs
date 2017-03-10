{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell   #-}
module Home where

import Foundation
import Yesod.Core
import Yesod.Form

import qualified Data.Map as Map
import Control.Concurrent.MVar
import System.Random
import qualified Data.Text as T
import Data.Conduit
import Data.Conduit.List
import Data.Monoid
import Control.Monad (join)
import Data.ByteString

import FrameCreator
import Utils

-- Spec
-- Upload the user given pattern and do error checking
-- If pattern is ok then give a link to the pattern id
-- Store the pattern in memory App

data FormData = FormData
  FileInfo Int ForeGroundTemplate

form = renderDivs $ FormData
  <$> areq fileField "Pattern File" Nothing
  <*> areq intField "Count" (Just 8)
  <*> areq (radioField optionsEnum) "Template"
        (Just Horizontal)

getHomeR :: Handler Html
getHomeR = do
    ((_, widget), enctype) <- runFormPost form
    defaultLayout [whamlet|$newline never
      <form method=post enctype=#{enctype}>
        ^{widget}
        <p>
        <input type=submit>Submit
|]

postHomeR :: Handler Html
postHomeR = do
  ((result, widget), enctype) <- runFormPost form
  $logDebug "Trying to read uploaded file"

  fd <- case result of
    FormSuccess (FormData f c t) -> do
      -- Get all file data
      d <- runConduit
        ((fileSource f) =$= (Data.Conduit.List.fold (<>) ""))

      return $ Just (d,c,t)
    _ -> return Nothing

  let patData = join $ parseImageData <$> fd

  case fd of
    Nothing -> $logError $ "Could not read input file"
    Just _ -> return ()

  case patData of
    Nothing -> do
      $logError $ "Could not parse file"
      redirect HomeR
      -- Set message and return to home

    Just pd -> do -- Store pd and go to preview
      $logInfo $ "Parse succesful"

      appSt <- getYesod

      patID <- liftIO $ addToMVarMap (patternDB appSt) PatternID pd

      redirect $ PreviewPatternR patID
