import Application () -- for YesodDispatch instance
import Foundation
import Yesod.Core

import Control.Concurrent.MVar
import Control.Monad (join)
import qualified Data.Map as Map

main :: IO ()
main = join $
  warp 3000 <$>
    (App <$> newMVar (Map.empty)
      <*> newMVar (Map.empty)
      <*> newMVar (Map.empty)
      <*> newMVar (Map.empty))
