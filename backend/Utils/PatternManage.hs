module Utils.PatternManage where

import Import
import System.Directory
import System.FilePath.Posix
import Diagrams.TwoD.Image
import Diagrams.Backend.Rasterific
import Diagrams.Prelude hiding (render)
import Message
import Common

import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NE
import System.Random

import qualified Data.Text as T

-- getPatternList :: IO (Response)
getPatternList = do
  groups <- listDirectory patternsDir

  PatternList <$> forM groups
    (\folder -> do
      files <- listDirectory (patternsDir </> folder)
      return (T.pack folder, map T.pack $
                filter (\f -> takeExtension f == ".png") files))

getPatternDia :: PatternName -> IO (Maybe (Diagram Rasterific))
getPatternDia (d,f) = do
  e <- loadImageEmb (T.unpack (d <> f))
  return $ case e of
    Left _ -> Nothing
    Right dimg -> Just $ image dimg

getPatternsDia :: NonEmpty PatternName ->
  IO (Maybe (NonEmpty (Diagram Rasterific)))
getPatternsDia pats = do
  dias <- mapM getPatternDia (NE.toList pats)
  let patsFound = all isJust dias
      p = NE.fromList $ catMaybes dias

  if (not patsFound)
    then return $ Just p
    else return Nothing

savePng ::
     Maybe (Text, Text) -- File name
  -> ByteString
  -> IO (Text)  -- File name
savePng names bs = do
  fullFileName <- case names of
    Just (dir', fileName') -> do

      let fullFileName = dir </> fileName ++ ".png"
          dir = T.unpack dir'
          fileName = T.unpack fileName'
      createDirectoryIfMissing True dir

      return (fullFileName)
    Nothing -> do
      rnd <- randomRIO (0,maxBound::Int)
      let fullFileName = previewDir ++ fileName
          fileName = show rnd ++ ".png"
      return fullFileName
  writeFile fullFileName bs
  return $ T.pack $ fullFileName
