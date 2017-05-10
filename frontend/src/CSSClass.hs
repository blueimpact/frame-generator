{-# LANGUAGE OverloadedStrings #-}
module CSSClass where

import qualified Data.Text as T

combine = T.intercalate " "

editFgtWidget = combine
  []

editFgtWidgetPatternBrowse = combine
  ["pre-scrollable"]

previewImg = combine
  ["preview-img", "img-thumbnail"]
