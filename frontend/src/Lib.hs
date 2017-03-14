{-# LANGUAGE TemplateHaskell #-}

module Lib where


import Data.Text (Text)
import qualified Data.Text as T
import Data.FileEmbed

import Reflex
import Reflex.Dom

main = mainWidgetWithCss  $(embedFile "src/style.css") (undefined)
