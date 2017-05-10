{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE FlexibleContexts #-}

module Utils
  where

import Reflex.Dom
import Data.Text (Text)
import qualified Data.Text as T

import qualified GHCJS.Types as JS
import qualified GHCJS.DOM.Types as JS
import qualified GHCJS.Foreign as JS
import qualified GHCJS.Marshal.Pure as JS (pFromJSVal)

import qualified GHCJS.DOM.Blob as Blob
import qualified JavaScript.Array as JA

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL

import Reflex.Dom.Contrib.Utils
import Reflex.Dom.Contrib.Widgets.EditInPlace
import Data.Aeson
import Data.Monoid
import qualified CSSClass as C
import qualified Data.Map as Map
import Data.Maybe

foreign import javascript unsafe "window['URL']['createObjectURL']($1)" createObjectURL_ :: Blob.Blob -> IO JS.JSVal

enc :: (ToJSON a, Functor f) => f a -> f [ByteString]
enc mes = (:[]) <$> BSL.toStrict <$> encode <$> mes

createObjectURL :: ByteString -> IO Text
createObjectURL bs = do
  let
    run f payload = BS.useAsCString payload $ \cStr -> do
      -- Defined in Reflex.Dom
      --foreign import javascript unsafe "new Uint8Array($1_1.buf, $1_2, $2)" extractByteArray :: Ptr CChar -> Int -> IO JS.JSVal
      ba <- extractByteArray cStr $ BS.length payload
      let opt :: Maybe JS.BlobPropertyBag
          opt = Nothing
          baBlob = (JS.pFromJSVal ba) :: JS.Blob
      b <- Blob.newBlob [baBlob] opt
      f b
  url <- run createObjectURL_ bs
  return $ T.pack $ JS.fromJSString $ JS.pFromJSVal url

buttonE txt c = do
  ev <- button txt
  return (c <$ ev)

img url = imgJump Nothing url

imgJump jumpTag url = do
  let href = fromMaybe (Map.empty) ((\t -> ("href" =: t)) <$> jumpTag)
  (e,_) <- elAttr' "a" (href <> ("class" =: "thumbnail")) $
    elAttr ("img") (("src" =: url)) blank
  return e

rangeInputWidgetWithTextEditAndReset ::
  (MonadWidget t m)
  => Text                   -- Label
  -> Double                 -- Initial value
  -> (Float, Float, Float)  --
  -> Event t ()             -- Update reset value, when saving
  -> m (RangeInput t)

rangeInputWidgetWithTextEditAndReset
  label initVal' (min, max, step) resetUpd = do

  rec
    let
      initVal = realToFrac initVal'
      val = tshow <$> _rangeInput_value ri

      setValEv1 = read <$> (T.unpack <$> e)
      setValEv2 = tagPromptlyDyn resetValDyn r

      setValEv = leftmost [setValEv1, setValEv2]

    resetValDyn <- holdDyn initVal
      (tagPromptlyDyn (_rangeInput_value ri) resetUpd)

    (e,r) <- el "tr" $ do
      el "table" $ do
        el "td" $ text label
        evValChange <- el "td" $
          editInPlace (constant True) val
        resetEv <- el "td" $
          button "Reset"
        return (evValChange, resetEv)

    ri <- el "tr" $ rangeInput $
      RangeInputConfig initVal setValEv
        (constDyn $ ("min" =: tshow min) <> ("max" =: tshow max)
          <> ("step" =: tshow step))

  return ri

idTag t = elAttr "div" (("id" =: t) <> ("style" =: "padding-top: 70px"))

data PaginationEvents =
  Prev | Next | Page Int
  deriving (Show)

doPagination ::
  (MonadWidget t m)
  => Int  -- Number of items in a page
  -> Dynamic t [a] -- List of items
  -> ([a] -> m (Event t b)) -- Render widget
  -> m (Event t b)
doPagination numOfPages dynList widget = do
  let
    lists = makeLists <$> dynList
    makeLists [] = []
    makeLists ls = [a] ++ makeLists b
      where (a,b) = splitAt numOfPages ls
    pages = Map.fromList <$> ((zip [1..]) <$> lists)
    pagesCount = length <$> lists

  rec
    let currentPage = (\(ps,p) ->
                         if Map.null ps
                           then []
                           else ps Map.! p) <$> (zipDyn pages page)
    valEv <- dyn (widget <$> currentPage)
    page <- paginationBar pagesCount
  val <- switchPromptly never valEv
  return val

paginationBar ::
  (MonadWidget t m)
  => Dynamic t Int -- Number of pages
  -> m (Dynamic t Int) -- Selected Page
paginationBar dynSize = do
  rec
    ev <- elAttr "nav" (("aria-label" =: "Page navigation")) $
      elClass "ul" "pager" $ do
        ep <- el "li" $ do
          (e,_) <- elAttr' "a" (("aria-label" =: "Previous")) $
            elAttr "span" ("aria-hidden" =: "true") $ text "«"
          return $ Prev <$ domEvent Click e
        let
          f sel page = do
            let c = if page == sel then "Active" else ""
            elClass "li" c $ do
              (e,_) <- elAttr' "a" Map.empty $
                text $ tshow page
              return $ Page page <$ domEvent Click e
          pages = (((flip take) [1..]) <$> dynSize)
          dynVal = (zipDyn pages dynPage)
        evEvs <- dyn ((\(ps, s) -> mapM (f s) ps) <$> dynVal)
        e <- switchPromptly never (leftmost <$> evEvs)

        en <- el "li" $ do
          (e,_) <- elAttr' "a" (("aria-label" =: "Next")) $
            elAttr "span" ("aria-hidden" =: "true") $ text "»"
          return $ Next <$ domEvent Click e

        return $ leftmost $ [ep,e,en]

    let handler (s,Prev) i = if i > 1 then Just (i - 1) else Nothing
        handler (s,Next) i = if i < s then Just (i + 1) else Nothing
        handler (s,(Page p)) i = if i /= p then Just p else Nothing
    dynPage <- foldDynMaybe handler 1 (attachPromptlyDyn dynSize ev)

  return dynPage
