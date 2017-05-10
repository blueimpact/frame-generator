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
import qualified Data.List
import Data.Text (Text)
import qualified Data.Text as T
import Data.Aeson
import Database.Persist.Sql (fromSqlKey, toSqlKey)

import System.Random

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
                      <$> (ForeGroundData <$> d)
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

        (Just (PreviewForeGroundTemplate fgtId l1 l2 l3)) -> do
          fgt <- mylift $ runDB $ get $ toSqlKey fgtId

          let
            l = (NE.map snd) <$> pl
            pl :: Maybe (NonEmpty (PatternName, ForeGroundParams))
            pl = join $ decodeStrict <$>
              foreGroundTemplateDBData <$> fgt

            perms = getAllPermutations l1 l2 l3

            -- Apply the list of patterns to all layers
            getPreview ::
              NonEmpty (Text,Text)
              -> IO (Maybe (FgtId, NonEmpty (Text,Text), Text))
            getPreview pats = do
              dias <- liftIO $ getPatternsDiaScaled pats

              let resDia = getForeGround <$> dias <*> l
                  resImg = encodeToPng <$> resDia <*> pure 600
              fname <- liftIO $ forM resImg (savePng Nothing)
              return $ (\f -> (fgtId,pats,f)) <$> fname

          lst <- liftIO $ forM perms (getPreview)
          let msg = ForeGroundListPreviewT (ForeGroundListPreview
                      (catMaybes (NE.toList lst)))
          return $ Just msg

        (Just (SaveForeGround (ForeGroundData fgtData))) -> do
          let pats = NE.map fst fgtData
              l = NE.map snd fgtData

          rnd <- liftIO $ randomRIO (0,maxBound::Int64)
          fNames <- liftIO $ makeFGAndSave rnd pats l

          let
              fname = fst <$> fNames
              maskName = snd <$> fNames
              d = enc (ForeGroundData fgtData)
          let fg = ForeGroundDB d <$> fname <*> maskName
          mylift $ runDB $ mapM insert fg

          return Nothing

        (Just (ApplyForeGroundTemplate fgtId pats)) -> do
          fgt <- mylift $ runDB $ get $ toSqlKey fgtId

          let
              l = (NE.map snd) <$> pl
              pl :: Maybe (NonEmpty (PatternName, ForeGroundParams))
              pl = join $ decodeStrict <$>
                    foreGroundTemplateDBData <$> fgt

              d = enc <$> (ForeGroundData <$> (NE.zip pats <$> l))
          fNames <- liftIO $ mapM (makeFGAndSave fgtId pats) l

          let fg = ForeGroundDB <$> d <*> fname <*> maskName
              fname = fst <$> join fNames
              maskName = snd <$> join fNames

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
          key <- mylift $ runDB $ do
            fg <- get $ ((toSqlKey fgId) :: Key ForeGroundDB)

            let
                new = ForeGroundTemplateDB <$> (foreGroundDBData <$> fg)

            forM new insert

          let msg = NewForeGroundTemplateT
                <$> NewForeGroundTemplate
                <$> (fromSqlKey <$> key)
          return $ msg

        (Just (DeleteForeGround fgIds)) -> do
          mylift $ runDB $ forM fgIds (\i -> delete (( toSqlKey i) :: Key ForeGroundDB))
          return Nothing

        (Just (DownloadForeGroundPng fgIds)) ->do
          files <- mylift $ runDB $ forM fgIds
            (\i -> do
              fg <- get (( toSqlKey i) :: Key ForeGroundDB)
              return $ (\a b -> [a,b]) <$> (foreGroundDBPngPreview <$> fg)
                <*> (foreGroundDBMaskFile <$> fg)
            )
          zipFile <- liftIO $ getZipFile (concat $ catMaybes files)
          return $ DownloadForeGroundPngLinkT
            <$> (DownloadForeGroundPngLink <$> zipFile)
        Nothing -> return Nothing


getAllPermutations ::
     NonEmpty (Text,Text)
  -> Maybe (NonEmpty (Text,Text))
  -> Maybe (NonEmpty (Text,Text))
  -> NonEmpty (NonEmpty (Text,Text))
-- Get all permutations for given number of layers from the groups
getAllPermutations l1 l2 l3 =
  case (l2, l3) of
    (Just y, Just z) -> threeLayers l1List (NE.toList y) (NE.toList z)
    (Just y, Nothing) -> twoLayers l1List (NE.toList y)
    _ -> NE.fromList $ map (\x -> NE.fromList [x]) l1List
  where
    l1List = NE.toList l1
    threeLayers xl yl zl =
      NE.fromList ([(NE.fromList [x, y, z]) | x <- xl, y <- yl, z <- zl])
    twoLayers xl yl = NE.fromList ([(NE.fromList [x, y]) | x <- xl, y <- yl])

makeFGAndSave ::
     FgtId
  -> NonEmpty (Text,Text)
  -> NonEmpty ForeGroundParams
  -> IO (Maybe (Text,Text))
makeFGAndSave fgtId pats l = do
  dias <- getPatternsDiaScaled pats

  let resDia = getForeGround <$> dias <*> pure l
      resImg = encodeToPng <$> resDia <*> pure 600
      m = getMask 600 def
        <$> resDia
      grps = map fst pats
      name = concat $ intersperse "_" $ NE.cons (tshow fgtId) fnames
      fnames = (map ((T.dropEnd 4).snd) pats) -- remove .png
      dir = (T.concat $ NE.toList $ NE.intersperse "_" grps) <> "/"

      dirSave = foregroundDir <> dir

  fname <- forM resImg (savePng (Just (dirSave,name)))

  maskName <- forM m (savePng (Just (dirSave,name <> "_mask")))

  return $ (,) <$> fname <*> maskName
