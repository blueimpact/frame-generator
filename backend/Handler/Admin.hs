module Handler.Admin where

import Import
import qualified Data.Map as Map
import AppData

getAdminR :: Handler Html
getAdminR = do
    defaultLayout [whamlet|
      <p>Careful!
      <p>
        <a href=@{AdminResetR}>Do Reset
        |]

getAdminResetR :: Handler Html
getAdminResetR = do
  appSt <- appData <$> getYesod

  liftIO $ do
    swapMVar (patternDB appSt) Map.empty
    swapMVar (foreGroundDB appSt) Map.empty
    swapMVar (pngDB appSt) Map.empty
    swapMVar (imageDB appSt) Map.empty
  
  redirect HomeR
