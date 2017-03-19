{-# LANGUAGE CPP #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}

module Todo.Vanilla
  ( Element(..)
  , Selector
  , consoleLog
  , alert
  , onload
  , addClass
  , removeClass
  , setHtml
  , docElement
  , elementsOf
  , elementOf
  , getValue
  , setValue
  , itemId
  , focus
  , checked
  , keyCode
  , getHash
  , on
  , onWindow
  , delegate
  , JSVal
  , toJSVal
  ) where

import Clay.Render (renderSelector)
import Clay.Selector (Selector)
import Data.Char
import Data.Foldable
import Data.Maybe (fromJust)
import Data.Typeable
import Lucid
import Protolude hiding (Selector, on)
import Unsafe.Coerce (unsafeCoerce)

import qualified Data.Text.Lazy as LText
import qualified Text.Read as Text

import GHCJS.Types
import GHCJS.Marshal
import GHCJS.Foreign
import GHCJS.Foreign.Callback
import GHCJS.Nullable (maybeToNullable, nullableToMaybe, Nullable)
import Data.JSString.Text (textToJSString, textFromJSString, lazyTextToJSString, lazyTextFromJSString)

data Element = Element JSVal

fromHtml = lazyTextToJSString . renderText

fromSel = lazyTextToJSString . renderSelector

-- * the big 3
-- | console.log
foreign import javascript unsafe
    "console.log($1)"
    js_consoleLog :: JSString -> IO ()

consoleLog :: Text -> IO ()
consoleLog a = js_consoleLog (textToJSString a)

foreign import javascript unsafe
  "alert($1)" js_alert :: JSString -> IO ()

alert :: Text -> IO ()
alert t = js_alert (textToJSString t)

foreign import javascript unsafe "window.onload = $1"
  js_onload :: Callback (IO ()) -> IO ()

onload :: IO () -> IO ()
onload f = do
    f_ <- asyncCallback f
    js_onload f_


-- * element manipulation
foreign import javascript unsafe
    "$1.classList.add($2)"
    js_classListAdd :: JSVal -> JSString -> IO ()

addClass :: Element -> Selector -> IO ()
addClass (Element el) sel =
    js_classListAdd el (fromSel sel)

foreign import javascript unsafe
    "$1.classList.remove($2)"
    js_classListRemove :: JSVal -> JSString -> IO ()

removeClass :: Element -> Selector -> IO ()
removeClass (Element el) sel =
    js_classListRemove el (fromSel sel)

foreign import javascript unsafe
    "$1.innerHTML = $2"
    js_innerHtml :: JSVal -> JSString -> IO ()

setHtml :: Element -> Html () -> IO ()
setHtml (Element el) html =
    js_innerHtml el (fromHtml html)

foreign import javascript unsafe
    "document.querySelector($1)"
    js_docQuerySelector :: JSString -> IO JSVal

-- | element selection
docElement :: Selector -> IO Element
docElement sel =
    Element <$> js_docQuerySelector (fromSel sel)

foreign import javascript unsafe
    "$1.querySelectorAll($2)"
    js_querySelectorAll :: JSVal -> JSString -> IO JSVal

elementsOf :: Element -> Selector -> IO JSVal
elementsOf (Element el) sel = js_querySelectorAll el (fromSel sel)

foreign import javascript unsafe
    "$1.querySelector($2)"
    js_querySelector :: JSVal -> JSString -> IO JSVal

elementOf :: Element -> Selector -> IO JSVal
elementOf (Element el) sel = js_querySelector el (fromSel sel)

foreign import javascript unsafe
    "$1.value"
    js_getValue :: JSVal -> IO JSString

getValue :: Element -> IO Text
getValue (Element el) = textFromJSString <$> js_getValue el

foreign import javascript unsafe
    "$1.value = $2"
    js_setValue :: JSVal -> JSString -> IO ()

setValue :: Element -> Text -> IO ()
setValue (Element el) val = js_setValue el (textToJSString val)

foreign import javascript unsafe
    "$1.dataset.id"
    js_getDatasetId :: JSVal -> IO JSString

itemId :: Element -> IO (Maybe Int)
itemId (Element el) = do
    i <- js_getDatasetId el
    pure $ readMaybe (LText.unpack $ lazyTextFromJSString i)

foreign import javascript unsafe
    "$1.focus()"
    js_focus :: JSVal -> IO ()

focus :: Element -> IO ()
focus (Element el) = js_focus el

foreign import javascript unsafe
    "$1.item($2)"
    js_getItem :: JSVal -> Int -> IO (Nullable JSVal)

-- getItem :: NodeList -> Int -> IO (Maybe Node)
getItem :: JSVal -> Int -> IO (Maybe JSVal)
getItem list n =
  nullableToMaybe <$> js_getItem list n

foreign import javascript unsafe
    "$1.keyCode"
    js_keyCode :: JSVal -> IO Int

keyCode :: JSVal -> IO Int
keyCode ev = js_keyCode ev

foreign import javascript unsafe
    "document.location.hash"
    js_getHash :: IO JSString

getHash :: IO JSString
getHash = js_getHash

foreign import javascript unsafe
    "$1.checked"
    js_checked :: JSVal -> IO Bool

checked :: Element -> IO Bool
checked (Element el) = js_checked el

-- * event listening
foreign import javascript unsafe
    "$1.addEventListener($2,$3,$4)"
    js_addEventListener :: JSVal -> JSString -> Callback (JSVal -> IO ()) -> Bool -> IO ()

on :: Element -> Text -> (Element -> JSVal -> IO ()) -> IO () 
on (Element el) uiAction handler = do
  handler' <- asyncCallback1 (handler (Element el))
  js_addEventListener el (textToJSString uiAction) handler' False

foreign import javascript unsafe
    "window.addEventListener($1,$2,$3)"
    jsWindowAddEventListener :: JSString -> Callback (JSVal -> IO ()) -> Bool -> IO ()

-- null = maybeToNullable Nothing

onWindow :: LText.Text -> (JSVal -> IO ()) -> IO ()
onWindow uiAction handler = do
  handler' <- asyncCallback1 handler
  jsWindowAddEventListener
    (lazyTextToJSString uiAction)
    handler'
    False

delegate :: Element -> Selector -> Text -> (Element -> JSVal -> IO ()) -> IO ()
delegate (Element base) pattern0 ev action = do
  let useCapture = ev == "blur" || ev == "focus"
  dispatch <- asyncCallback1 $ applyIn (Element base) pattern0 action 
  js_addEventListener base (textToJSString ev) dispatch useCapture

foreign import javascript unsafe
    "$1.target"
    js_target :: JSVal -> IO JSVal

foreign import javascript unsafe
    "$1===$2"
    js_eq :: JSVal -> JSVal -> IO Bool

foreign import javascript unsafe
    "$1.length"
    js_length :: JSVal -> IO Int

applyIn :: Element -> Selector -> (Element -> JSVal -> IO ()) -> JSVal -> IO ()
applyIn base pattern0 action ev = do
  t <- js_target ev
  p <- elementsOf base pattern0
  n <- js_length p
  elem' <- findM
           (\x -> do
               item <- getItem p x
               case item of
                 Nothing -> pure False
                 Just item' -> js_eq t item') [0..n]
  case elem' of
    Nothing -> pure ()
    Just iElem -> do
      eitem <- getItem p iElem
      case eitem of
        Nothing -> pure ()
        Just eitem' -> do
            li <- findUp (Element eitem') "li"
            case li of
              Nothing -> print "no li parent???"
              Just li' -> action li' ev

foreign import javascript unsafe
    "$1.parentElement"
    js_parentElement :: JSVal -> IO (Nullable JSVal)

foreign import javascript unsafe
    "$1.tagName"
    js_tagName :: JSVal -> IO JSString

-- | go up the tree and find an element, if it exists
findUp :: Element -> LText.Text -> IO (Maybe Element)
findUp (Element base) sel = do
  parent <- nullableToMaybe <$> js_parentElement base
  case parent of
    Nothing -> pure Nothing
    Just parent' -> do
      name <- js_tagName parent'
      if (LText.toLower $ lazyTextFromJSString name) == (LText.toLower sel)
        then pure $ Just (Element parent')
        else findUp (Element parent') sel

findM :: (Monad m, Traversable t) => (a -> m Bool) -> t a -> m (Maybe a)
findM p = fmap (getFirst . fold) . mapM 
       (fmap First . (\x -> do
           p' <- p x
           pure $ if p' then Just x else Nothing))
