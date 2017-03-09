{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
module Frame where

import Foundation
import Yesod.Core
import FrameCreator

import Control.Concurrent.MVar
import qualified Data.Map as Map
import Data.ByteString
import System.Random

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
      rnd <- liftIO $ randomRIO (minBound::Int,maxBound)

      let pngData = getPngForPD pat 500
          pngID = PngID rnd

      liftIO $ modifyMVar_ (pngDB appSt)
        (\db -> return $ Map.insert pngID pngData db)

      defaultLayout [whamlet|$newline never
          <p>
          <img src=@{PngR pngID}>
|]

getPngR :: PngID -> Handler TypedContent
getPngR pngID = do
  appSt <- getYesod
  db <- liftIO $ readMVar (pngDB appSt)
  
  let pngData = Map.lookup pngID db
  case pngData of
    Nothing -> notFound
    Just d -> respondSource "image/png" (sendChunkBS d)

