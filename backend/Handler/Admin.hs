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
  redirect HomeR
