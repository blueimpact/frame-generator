{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE ViewPatterns      #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
module Foundation 
  ( module Foundation
  , module AppData
  )
  where

import Yesod.Core
import Yesod.Form

import AppData

mkYesodData "App" $(parseRoutesFile "routes")

instance Yesod App where
    shouldLog _ src level = True -- good for development
      -- level == LevelWarn || level == LevelError -- good for production

instance RenderMessage App FormMessage where
    renderMessage _ _ = defaultFormMessage
