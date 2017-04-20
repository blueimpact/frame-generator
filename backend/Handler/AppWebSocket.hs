{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PartialTypeSignatures #-}
module Handler.AppWebSocket where

import Import
import Yesod.WebSockets
import Utils.PatternManage
import Utils.Misc
import Utils.FrameCreator
import Message
import Common

import qualified Data.Map as Map
import Control.Monad
import Data.Conduit
import qualified Data.Conduit.List
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NE
import Data.Text (Text)
import qualified Data.Text as T
import Data.Aeson
import Database.Persist.Sql (fromSqlKey, toSqlKey)


getAppWebSocket :: Handler Html
getAppWebSocket = do
  appSt <- appData <$> getYesod
  webSockets (appWebSocketServer appSt)
  redirect HomeR

-- enc :: (ToJSON a, Functor f) => f a -> f [ByteString]
-- enc mes = (:[]) <$> BSL.toStrict <$> encode <$> mes

enc = BSL.toStrict . encode

appWebSocketServer appSt = do

  sourceWS $$ Data.Conduit.List.mapMaybeM
      (handleRequest )
    =$= sinkWSBinary
  where
    mylift :: Handler a -> _ a
    mylift a = lift $ a
    -- handleRequest :: (MonadIO m, MonadLogger m)
    --   => BSL.ByteString
    --   -> m (Maybe BSL.ByteString)
    handleRequest req' =
      case decode req' of
        (Just GetPatternList) -> do
          msg <- encode <$> liftIO getPatternList
          return $ Just msg

        (Just GetForeGroundTemplateList   ) -> do
          keys <- mylift $ runDB $ do
            selectKeysList
              ([] :: [Filter ForeGroundTemplateDB]) []

          let msg = encode $ ForeGroundTemplateList $  map fromSqlKey keys
          return $ Just msg

        (Just (CreateForeGroundTemplate pat)) -> do
          key <- mylift $ runDB $ do
            let layers = NE.fromList [(pat, defFGParams)]
                defFGParams =
                  ForeGroundParams 8 0 1.0 100
            insert (ForeGroundTemplateDB $ enc layers)
          let msg = encode $ NewForeGroundTemplate (fromSqlKey key)
          return $ Just msg

        (Just (EditForeGroundTemplate fgtId)) -> do
          fgt <- mylift $ runDB $ do
            get (toSqlKey fgtId)
          let msg = encode <$> (ForeGroundTemplateData <$> d)
              d = join $ decodeStrict <$>
                foreGroundTemplateDBData <$> fgt
          return $ msg

        (Just (CloneForeGroundTemplate fgtId)) -> do
          key <- mylift $ runDB $ do
            orig <- get ((toSqlKey fgtId) :: Key ForeGroundTemplateDB)
            forM orig insert

          let msg = encode $ NewForeGroundTemplate
                <$> (fromSqlKey <$> key)
          return $ Just msg

        (Just (DeleteForeGroundTemplate fgtId)) -> do
          keys <- mylift $ runDB $ do
            delete ((toSqlKey fgtId) :: Key ForeGroundTemplateDB)
            selectKeysList
              ([] :: [Filter ForeGroundTemplateDB]) []

          let msg = encode $ ForeGroundTemplateList $
                      map fromSqlKey keys
          return $ Just msg

        (Just (PreviewForeGroundTemplate fgtId patsList)) -> do
          fgt <- mylift $ runDB $ get $ toSqlKey fgtId

          let
            getPreview pats = do
              dias <- liftIO $ getPatternsDia pats

              let resDia = getForeGround <$> dias <*> l
                  resImg = encodeToPng <$> resDia <*> pure 600
                  l = join $ decodeStrict <$>
                    foreGroundTemplateDBData <$> fgt
              fname <- forM resImg (savePng Nothing)
              return $ (\f -> (fgtId,pats,f)) <$> fname

          lst <- liftIO $ forM patsList getPreview
          let msg = encode $ ForeGroundListPreview $
                      catMaybes lst
          return $ Just msg

        (Just (ApplyForeGroundTemplate fgtId pats)) -> do
          fgt <- mylift $ runDB $ get $ toSqlKey fgtId

          dias <- liftIO $ getPatternsDia pats

          let resDia = getForeGround <$> dias <*> l
              resImg = encodeToPng <$> resDia <*> pure 600
              l = join $ decodeStrict <$>
                foreGroundTemplateDBData <$> fgt
              m = getMask 600 (MaskParams 4 4)
                <$> resDia

              grps = map fst pats
              patNames = concat $ map snd pats
              name = T.intersperse '_' patNames
              dir = T.concat $ NE.toList $ NE.intersperse "_" grps

              d = enc $ ForeGroundData fgtId pats

          fname <- liftIO $ forM resImg (savePng (Just (dir,name)))

          maskName <- liftIO $ forM m (savePng (Just (dir,name <> "_mask")))

          let fg = ForeGroundDB d <$> fname <*> maskName
          mylift $ runDB $ mapM insert fg

          return Nothing

        (Just GetForeGroundList           ) -> do
          objs <- mylift $ runDB $ do
            selectList
              ([] :: [Filter ForeGroundDB]) []

          let msg = encode $ ForeGroundList $
                      map g objs
              g (Entity k (ForeGroundDB _ p _)) = (fromSqlKey k, p)
          return $ Just msg

        (Just (EditForeGround fgId)) -> do
          fg <- mylift $ runDB $ get $ toSqlKey fgId

          let msg = encode <$> ForeGroundDataRes <$>
                  (join $ decodeStrict <$> (foreGroundDBData <$> fg))

          return msg

        (Just (DeleteForeGround fgId)) -> do
          mylift $ runDB $ delete (( toSqlKey fgId) :: Key ForeGroundDB)
          return Nothing

        (Just (DownloadForeGroundPng _)) -> return Nothing
        _ -> return Nothing
