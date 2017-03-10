module Utils where

import System.Random
import Data.Map (Map)
import qualified Data.Map as Map
import Control.Concurrent.MVar

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
          rnd <- randomRIO (0,maxBound::Int)
          let i = f rnd
          case Map.lookup i db of
            Nothing -> return $
              (Map.insert i v db, i)
            Just _ -> tryInsert

      tryInsert)

