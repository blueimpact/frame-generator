{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
module Home where

import Foundation
import Yesod.Core
import Yesod.Form

form = renderDivs $ (,) <$> fileAFormReq "File" <*> fileAFormOpt "Optional file"

getHomeR :: Handler Html
getHomeR = do
    ((_, widget), enctype) <- runFormPost form
    defaultLayout [whamlet|$newline never
      <form method=post enctype=#{enctype}>
        ^{widget}
        <p>
        <input type=submit>
|]

postHomeR :: Handler Html
postHomeR = do
    ((result, widget), enctype) <- runFormPost form
    let msubmission = case result of
            FormSuccess res -> Just res
            _ -> Nothing
    defaultLayout $ do
        [whamlet|$newline never
          $maybe (firstfile, second) <- msubmission
              <p>File received: #{fileName firstfile}
              $maybe secondfile <- second
                  <p>Second file received: #{fileName secondfile}
          <form method=post enctype=#{enctype}>
              ^{widget}
              <p>
              <input type=submit>
|]
