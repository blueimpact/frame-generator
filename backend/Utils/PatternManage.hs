module Utils.PatternManage where

import Import
import System.Directory
import System.FilePath.Posix
import Message

import qualified Data.Text as T

patternRoot = "static/patterns"

-- getPatternList :: IO (Response)
getPatternList = do
  groups <- listDirectory patternRoot

  PatternList <$> forM groups
    (\folder -> do
      files <- listDirectory (patternRoot </> folder)
      return (T.pack folder, map T.pack $
                filter (\f -> takeExtension f == ".png") files))
