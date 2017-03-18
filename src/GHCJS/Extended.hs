{-# LANGUAGE CPP #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}

module GHCJS.Extended 
  ( Element
  , Selector
  , addClass
  , removeClass
  , JSVal
  , toJSVal
  ) where

import Clay.Render (renderSelector)
import Clay.Selector (Selector)
import Control.Error.Extended
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
import Data.JSString.Text (textToJSString, textFromJSString, lazyTextToJSString, lazyTextFromJSString)

data Element = Element JSVal

fromHtml = lazyTextToJSString . renderText

fromSel = lazyTextToJSString . renderSelector

-- * ubiquitous
-- | console.log
foreign import javascript unsafe
    "console.log($1)"
    js_consoleLog :: JSString -> IO ()

consoleLog :: Text -> IO ()
consoleLog a = js_consoleLog (textToJSString a)


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


{-

-- | element selection
element :: Selector -> IO (Maybe Element)
element sel = do
    d <- currentDocument
    maybe (pure Nothing)
        (\x -> Doc.querySelector x (fromSel sel)) d

-- | either version
element' :: Selector -> ExceptT Text IO Element 
element' sel = do
  sel'  <- lift $ element sel
  failWith ("element selector " <> show sel <> " failed") sel'

elementsOf :: Element -> Selector -> IO (Maybe NodeList) 
elementsOf el sel = querySelectorAll el (fromSel sel)

elementOf :: Element -> Selector -> IO (Maybe Element) 
elementOf el sel = do
    el <- element sel
    maybe (pure Nothing)
        (\x -> querySelector x (fromSel sel)) el

focusElement :: Element -> IO ()
focusElement el = focus el


foreign import javascript unsafe "$1[\"value\"]" js_getValue :: Element -> IO JSString

getValue :: Element -> IO Text
getValue el = textFromJSString <$> js_getValue el

foreign import javascript unsafe "$1[\"value\"] = $2" js_setValue :: Element -> JSString -> IO ()

setValue :: Element -> Text -> IO ()
setValue el val = js_setValue el (textToJSString val)

foreign import javascript unsafe
    "$1.dataset.id"
    js_getDatasetId :: Element -> IO (Nullable Int)

itemId :: Element -> IO (Maybe Int)
itemId el = do
  nullableToMaybe <$> js_getDatasetId el

getItem :: NodeList -> Int -> IO (Maybe Node)
getItem list n =
  item list (fromIntegral n)

keyCode :: JSVal -> IO Int
keyCode ev = getKeyCode ev

getHash :: IO (Maybe JSString)
getHash = do
    d <- currentDocument
    case d of
      Nothing -> pure Nothing
      Just d' -> do
          l <- Doc.getLocation d'
          case l of
            Nothing -> pure Nothing
            Just l' -> do
                h <- Loc.getHash l'
                pure $ Just h

-- * event listening
foreign import javascript unsafe
    "$1.addEventListener($2,$3,$4)"
    js_addEventListener :: JSVal -> JSString -> (JSVal -> IO ()) -> Bool -> IO ()

{-
on :: (IsEvent ev) => Element -> Text -> (Element -> E.EventM Element ev ()) -> IO (IO ()) 
on el uiAction handler = do
    rel <- E.on el (E.EventName uiAction) (handler el)
    pure rel
-}

on :: Element -> Text -> (Element -> JSVal -> IO ()) -> IO () 
on el uiAction handler = do
  handler' <- syncCallback1 ContinueAsync (handler el)
  js_addEventListener el (toJSString uiAction) handler' False


-- up to here
onWindow = undefined
delegate = undefined

-- * common idioms
{-

-- * manipulating dom

-- * event listening
on :: JSVal -> String -> (JSVal -> Event -> IO ()) -> IO () 
on el uiAction handler = do
  handler' <- sync1 (handler el)
  jsAddEventListener el (toJSString uiAction) handler' (toJSBool False)


delegate :: Element -> Selector -> String -> (Element -> Event -> IO ()) -> IO ()
delegate base pattern0 ev action = do
  let useCapture = ev == "blur" || ev == "focus"
  dispatch <- sync1 $ applyIn base pattern0 action 
  jsAddEventListener base (toJSString ev) dispatch useCapture

applyIn :: Element -> Selector -> (Element -> Event -> IO ()) -> Event -> IO ()
applyIn base pattern0 action ev = do
  t <- jsTarget ev
  p <- jsQuerySelectorAll base (toJSString pattern0)
  n <- jsLength p
  elem' <- findM
           (\x -> do
               item <- getItem p x
               case item of
                 Nothing -> pure False
                 Just item' -> jsEq t item') [0..n]
  case elem' of
    Nothing -> pure ()
    Just iElem -> do
      Just eitem <- getItem p iElem
      li <- findUp eitem "li"
      case li of
        Nothing -> print "no li parent???"
        Just li' -> action li' ev



-- * miscellaneous


-- | go up the tree and find an element, if it exists
findUp :: Nullable Element -> Text.Text -> IO (Maybe Element)
findUp base sel = do
  parent <- nullableToMaybe <$> jsParentElement base
  case parent of
    Nothing -> pure Nothing
    Just parent' -> do
      name <- jsTagName parent'
      if (toLower <$> (fromJSString name)) == (toLower <$> (Text.unpack sel))
        then (pure $ Just parent')
        else (findUp parent' sel)

findM :: (Monad m, Traversable t) => (a -> m Bool) -> t a -> m (Maybe a)
findM p = fmap (getFirst . fold) . mapM 
       (fmap First . (\x -> do
           p' <- p x
           pure $ if p' then Just x else Nothing))


-}


foreign import javascript unsafe "$1.dataset.id" js_id :: JSVal -> IO JSString
foreign import javascript unsafe "$1.focus()" js_focus :: JSVal -> IO ()
foreign import javascript unsafe "$1.parentElement" js_parentElement :: JSVal -> IO JSVal
foreign import javascript unsafe "$1.querySelector($2)" js_elementQuerySelector :: JSVal -> JSString -> IO JSVal
foreign import javascript unsafe "$1.querySelectorAll($2)" js_querySelectorAll :: JSVal -> JSString -> IO JSVal
foreign import javascript unsafe "$1.tagName" js_tagName :: JSVal -> IO JSString
foreign import javascript unsafe "$1===$2" js_eq :: JSVal a -> JSVal -> IO Bool
foreign import javascript unsafe "$1.length" js_length :: JSVal -> IO Int
foreign import javascript unsafe "$1.value" js_getValue :: JSVal -> IO JSString
foreign import javascript unsafe "$1.value = $2" js_setValue :: JSVal -> JSString -> IO ()
foreign import javascript unsafe "$1.checked" js_checked :: JSVal -> IO Bool
foreign import javascript unsafe "$1.item($2)" js_item :: JSVal -> Word -> IO JSVal
foreign import javascript unsafe "$1.keyCode" js_keyCode :: JSVal -> IO Int
foreign import javascript unsafe "$1.target" js_target :: JSVal -> IO JSVal
foreign import javascript unsafe "console.log($1)" js_consoleLog :: JSString -> IO ()
foreign import javascript unsafe "document.location.hash" js_getHash :: IO JSString
foreign import javascript unsafe "document.querySelector($1)" js_querySelector :: JSString -> IO JSVal
foreign import javascript unsafe "window.addEventListener($1,$2,$3)" js_windowAddEventListener :: JSString -> (JSVal -> IO ()) -> Bool -> IO ()
foreign import javascript unsafe "window.onload = $1" js_onload :: CallBack (IO ()) -> IO ()

-}

