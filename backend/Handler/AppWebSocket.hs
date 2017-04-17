{-# LANGUAGE OverloadedStrings #-}
module Handler.AppWebSocket where

import Import
import Yesod.WebSockets
import Utils.PatternManage
import Utils.Misc
import Message
import AppData

import qualified Data.Map as Map
import Control.Monad
import Data.Conduit
import qualified Data.Conduit.List
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import Data.Aeson


getAppWebSocket :: Handler Html
getAppWebSocket = do
  appSt <- appData <$> getYesod
  webSockets (appWebSocketServer appSt)
  redirect HomeR

-- enc :: (ToJSON a, Functor f) => f a -> f [ByteString]
-- enc mes = (:[]) <$> BSL.toStrict <$> encode <$> mes

appWebSocketServer appSt = do

  sourceWS $$ Data.Conduit.List.mapMaybeM
      (handleRequest )
    =$= sinkWSBinary
  where
    handleRequest :: (MonadIO m, MonadLogger m)
      => BSL.ByteString
      -> m (Maybe BSL.ByteString)
    handleRequest req' = do
      case decode req' of
        (Just GetPatternList) -> do
          msg <- encode <$> liftIO getPatternList
          return $ Just msg
        _ -> return Nothing
