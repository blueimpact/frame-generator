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

-- Spec
-- Upload the user given pattern and do error checking
-- If pattern is ok then give a link to the pattern id
-- Store the pattern in memory App

form = renderDivs $ (,) <$> areq fileField "Pattern File" Nothing  <*> areq intField "Count" (Just 8)

getHomeR :: Handler Html
getHomeR = do
    ((_, widget), enctype) <- runFormPost form
    defaultLayout [whamlet|$newline never
      <form method=post enctype=#{enctype}>
        ^{widget}
        <p>
        \ <img class="aligncenter" alt="beastie.png" src="data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAAM0AAADNCAMAAAAsYgRbAAAAGXRFWHRTb2Z0d2FyZQBBZG9iZSBJbWFnZVJlYWR5ccllPAAAABJQTFRF3NSmzMewPxIG//ncJEJsldTou1jHgAAAARBJREFUeNrs2EEKgCAQBVDLuv+V20dENbMY831wKz4Y/VHb/5RGQ0NDQ0NDQ0NDQ0NDQ0NDQ0NDQ0NDQ0NDQ0NDQ0NDQ0NDQ0PzMWtyaGhoaGhoaGhoaGhoaGhoxtb0QGhoaGhoaGhoaGhoaGhoaMbRLEvv50VTQ9OTQ5OpyZ01GpM2g0bfmDQaL7S+ofFC6xv3ZpxJiywakzbvd9r3RWPS9I2+MWk0+kbf0Hih9Y17U0nTHibrDDQ0NDQ0NDQ0NDQ0NDQ0NTXbRSL/AK72o6GhoaGhoRlL8951vwsNDQ0NDQ1NDc0WyHtDTEhDQ0NDQ0NTS5MdGhoaGhoaGhoaGhoaGhoaGhoaGhoaGposzSHAAErMwwQ2HwRQAAAAAElFTkSuQmCC" scale="0">
        <input type=submit>
|]

postHomeR :: Handler Html
postHomeR = do
  ((result, widget), enctype) <- runFormPost form
  $logDebug "Trying to read uploaded file"

  fd <- case result of
        FormSuccess (f,c) -> do
          d <- runConduit ((fileSource f) =$= (Data.Conduit.List.fold (<>) ""))
          return ((Just (d,c)) :: Maybe (ByteString, Int))
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

      rnd <- liftIO $ randomRIO (minBound::Int,maxBound)

      let patID = PatternID rnd

      -- Potential blocking call
      liftIO $ modifyMVar_ 
        (patternDB appSt)
        (\db -> return $ Map.insert patID pd db)

      redirect $ PreviewPatternR patID

--
--  defaultLayout $ do
--      [whamlet|$newline never
--        $maybe file <- outputImage
--            <p>File received: #{fileName file}
--        $nothing
--          <p>Please try again;
--          <a href=@{Home}>Homepage 
-- |]
