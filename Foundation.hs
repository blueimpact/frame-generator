{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE ViewPatterns      #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
module Foundation where

import Yesod.Core
import Yesod.Form

data App = App

mkYesodData "App" $(parseRoutesFile "routes")

instance Yesod App

instance RenderMessage App FormMessage where
    renderMessage _ _ = defaultFormMessage
