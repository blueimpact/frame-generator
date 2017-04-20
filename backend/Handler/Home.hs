{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell   #-}
module Handler.Home where

import Import

getHomeR :: Handler Html
getHomeR = redirect (StaticR editapp_index_html)
