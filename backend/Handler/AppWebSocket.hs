{-# LANGUAGE OverloadedStrings #-}
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
    =$= Data.Conduit.List.map encodeResponse
    =$= sinkWSBinary
  where
    encodeResponse :: ResponseT -> BSL.ByteString
    encodeResponse = encode
    mylift :: Handler a -> ReaderT r (HandlerT App IO) a
    mylift a = lift $ a
    -- handleRequest :: (MonadIO m, MonadLogger m)
    --   => BSL.ByteString
    --   -> m (Maybe BSL.ByteString)

    handleRequest req' =
      case decode req' of
        (Just GetPatternList) -> do
          lst <- liftIO getPatternList
          return $ Just $ PatternListT lst

        (Just GetForeGroundTemplateList   ) -> do
          keys <- mylift $ runDB $ do
            selectKeysList
              ([] :: [Filter ForeGroundTemplateDB]) []

          return $ Just $ ForeGroundTemplateListT $
            ForeGroundTemplateList $ map fromSqlKey keys

        (Just (CreateForeGroundTemplate pat)) -> do
          key <- mylift $ runDB $ do
            let layers = NE.fromList [(pat, def :: ForeGroundParams)]
            insert (ForeGroundTemplateDB $ enc layers)
          return $ Just $ NewForeGroundTemplateT $
            NewForeGroundTemplate (fromSqlKey key)

        (Just (EditForeGroundTemplate fgtId)) -> do
          fgt <- mylift $ runDB $ do
            get (toSqlKey fgtId)
          let msg = ForeGroundTemplateDataT
                <$> ForeGroundTemplateDataRes fgtId
                      <$> (ForeGroundTemplateData <$> d)
              d = join $ decodeStrict <$>
                foreGroundTemplateDBData <$> fgt
          return $ msg

        (Just (CloneForeGroundTemplate fgtId)) -> do
          key <- mylift $ runDB $ do
            orig <- get ((toSqlKey fgtId) :: Key ForeGroundTemplateDB)
            forM orig insert

          let msg = NewForeGroundTemplateT
                <$> NewForeGroundTemplate
                <$> (fromSqlKey <$> key)
          return $ msg

        (Just (DeleteForeGroundTemplate fgtId)) -> do
          keys <- mylift $ runDB $ do
            delete ((toSqlKey fgtId) :: Key ForeGroundTemplateDB)
            selectKeysList
              ([] :: [Filter ForeGroundTemplateDB]) []

          let msg = ForeGroundTemplateListT $ ForeGroundTemplateList $
                      map fromSqlKey keys
          return $ Just msg

        (Just (DefaultPreview fgtId)) -> do
          let msg = ForeGroundListPreviewT $ ForeGroundListPreview $
                      []
          return $ Just msg

        (Just (PreviewForeGroundTemplate fgtId patsList)) -> do
          fgt <- mylift $ runDB $ get $ toSqlKey fgtId

          let
            getPreview pats = do
              dias <- liftIO $ getPatternsDiaScaled pats

              let resDia = getForeGround <$> dias <*> l
                  resImg = encodeToPng <$> resDia <*> pure 600
                  l = (NE.map snd) <$> pl
                  pl :: Maybe (NonEmpty (PatternName, ForeGroundParams))
                  pl = join $ decodeStrict <$>
                    foreGroundTemplateDBData <$> fgt
              fname <- liftIO $ forM resImg (savePng Nothing)
              return $ (\f -> (fgtId,pats,f)) <$> fname

          lst <- forM patsList getPreview
          let msg = ForeGroundListPreviewT $ ForeGroundListPreview $
                      catMaybes $ NE.toList lst
          return $ Just msg

        (Just (ApplyForeGroundTemplate fgtId pats)) -> do
          fgt <- mylift $ runDB $ get $ toSqlKey fgtId

          dias <- liftIO $ getPatternsDia pats

          let resDia = getForeGround <$> dias <*> l
              resImg = encodeToPng <$> resDia <*> pure 600
              l = (NE.map snd) <$> pl
              pl :: Maybe (NonEmpty (PatternName, ForeGroundParams))
              pl = join $ decodeStrict <$>
                    foreGroundTemplateDBData <$> fgt
              m = getMask 600 def
                <$> resDia

              grps = map fst pats
              name = concat $ intersperse "_" $ NE.cons (tshow fgtId) fnames
              fnames = (map ((T.dropEnd 4).snd) pats) -- remove .png
              dir = (T.concat $ NE.toList $ NE.intersperse "_" grps) <> "/"

              d = enc $ ForeGroundData fgtId pats
              dirSave = foregroundDir <> dir

          fname <- liftIO $ forM resImg (savePng (Just (dirSave,name)))

          maskName <- liftIO $ forM m (savePng (Just (dirSave,name <> "_mask")))

          let fg = ForeGroundDB d <$> fname <*> maskName
          mylift $ runDB $ mapM insert fg

          return Nothing

        (Just GetForeGroundList           ) -> do
          objs <- mylift $ runDB $ do
            selectList
              ([] :: [Filter ForeGroundDB]) []

          let msg = ForeGroundListT $ ForeGroundList $
                      map g objs
              g (Entity k (ForeGroundDB _ p _)) = (fromSqlKey k, p)
          return $ Just msg

        (Just (EditForeGround fgId)) -> do
          fg <- mylift $ runDB $ get $ toSqlKey fgId

          let msg = ForeGroundDataResT <$> ForeGroundDataRes <$>
                  (join $ decodeStrict <$> (foreGroundDBData <$> fg))

          return msg

        (Just (DeleteForeGround fgId)) -> do
          mylift $ runDB $ delete (( toSqlKey fgId) :: Key ForeGroundDB)
          return Nothing

        (Just (DownloadForeGroundPng _)) -> return Nothing
        Nothing -> return Nothing
