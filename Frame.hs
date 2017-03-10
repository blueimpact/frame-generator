{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
module Frame where

import Foundation
import Yesod.Core
import FrameCreator

import Control.Concurrent.MVar
import qualified Data.Map as Map
import Data.Map (Map)
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

      let pngData = encodeToPng (origPatternData pat) 500
          pngID = PngID rnd

      liftIO $ modifyMVar_ (pngDB appSt)
        (\db -> return $ Map.insert pngID pngData db)

      defaultLayout [whamlet|$newline never
        <p>
        <img src=@{PngR pngID}>
        <a href=@{MakeForeGroundR patID}>Make Foreground
|]

-- Add the value to a random key and returns the key
addToMVarMap :: (Ord k) =>
     MVar (Map k v)
  -> (Int -> k)
  -> v
  -> IO k
addToMVarMap mvar f v = do
  modifyMVar mvar
    (\db -> do
      let
        -- tryInsert :: (Ord k) => IO (Map k v, k)
        tryInsert = do
          rnd <- randomRIO (minBound::Int,maxBound)
          let i = f rnd
          case Map.lookup i db of
            Nothing -> return $
              (Map.insert i v db, i)
            Just _ -> tryInsert

      tryInsert)

getMakeForeGroundR :: PatternID -> Handler Html
getMakeForeGroundR patID = do
  appSt <- getYesod

  db <- liftIO $ readMVar (patternDB appSt)

  let pd = Map.lookup patID db

  case pd of
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

      defaultLayout [whamlet|$newline never
          <p>
          <img src=@{PngR pngID}>
|]

getEditForeGroundR :: ForeGroundID -> Handler Html
getEditForeGroundR fgID = undefined
--   appSt <- getYesod
--
--   db <- liftIO $ readMVar (patternDB appSt)
--
--   let pd = Map.lookup patID db
--
--   case pd of
--     Nothing -> redirect HomeR
--     Just pat -> do
--       rnd <- liftIO $ randomRIO (minBound::Int,maxBound)
--
--       let pngData = getPngForPD pat 500
--           pngID = PngID rnd
--
--       liftIO $ modifyMVar_ (pngDB appSt)
--         (\db -> return $ Map.insert pngID pngData db)
--
--       defaultLayout [whamlet|$newline never
--           <p>
--           <img src=@{PngR pngID}>
-- |]

getPngR :: PngID -> Handler TypedContent
getPngR pngID = do
  appSt <- getYesod
  db <- liftIO $ readMVar (pngDB appSt)

  let pngData = Map.lookup pngID db
  case pngData of
    Nothing -> notFound
    Just d -> respondSource "image/png" (sendChunkBS d)

