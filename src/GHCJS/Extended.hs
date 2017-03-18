{-# LANGUAGE CPP #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}

module GHCJS.Extended 
  ( 
    -- maybeJSNull 
    Element
  , Event
  , UIEvent
  , Selector
  , element
  , element'
  , elementOf
  , on
  , onWindow
  , delegate
  , addClass
  , removeClass
  , setValue
  , getValue
  , setHtml
  , itemId
  , focusElement
  , getHash
  , keyCode
  , maybeToNullable
  , nullableToMaybe
  ) where

import Clay.Render (renderSelector)
import Clay.Selector (Selector)
import Control.Error.Extended
import Data.Char
import Data.Foldable
import Data.Maybe (fromJust)
import Data.Typeable
import Lucid
import Prelude (String)
import Protolude hiding (Selector, on)
import Unsafe.Coerce (unsafeCoerce)

import qualified Data.Text.Lazy as LText
import qualified Text.Read as Text

import GHCJS.Types
import GHCJS.Marshal
import GHCJS.Foreign
import GHCJS.Foreign.Callback
import Data.JSString.Text (textToJSString, textFromJSString, lazyTextToJSString, lazyTextFromJSString)

import GHCJS.DOM

import GHCJS.DOM.Types hiding (Text)

import qualified GHCJS.DOM.EventM as E (EventM(..), on)
import GHCJS.DOM.UIEvent (UIEvent(..), getKeyCode)
import GHCJS.DOM.EventTarget (addEventListener)
import GHCJS.DOM.DOMTokenList (add, remove)
import GHCJS.DOM.NodeList (item)
import qualified GHCJS.DOM.Location as Loc (getHash)
import GHCJS.DOM.Element (getClassList, setInnerHTML, querySelectorAll, focus, querySelector, getDataset, getId)
import qualified GHCJS.DOM.Document as Doc (querySelector, getLocation)
import GHCJS.Types (JSString)
import GHCJS.DOM.Types (ToJSString(..), FromJSString(..))
import GHCJS.DOM.EventTargetClosures (EventName)

fromHtml = lazyTextToJSString . renderText

fromSel = lazyTextToJSString . renderSelector

-- * common syncing patterns in calling javascript functions from haskell

-- | no arguments
sync :: IO () -> IO (Callback (IO ()))
sync f = syncCallback ContinueAsync f

-- | one (javascript value) argument
sync1 :: (JSVal -> IO ()) -> IO (Callback (JSVal -> IO ()))
sync1 f = syncCallback1 ContinueAsync f

-- * element manipulation

addClass :: Element -> Selector -> IO ()
addClass el sel = do
    l <- getClassList el
    maybe (pure ()) (\x -> add x [fromSel sel]) l

removeClass :: Element -> Selector -> IO ()
removeClass el sel =  do
    l <- getClassList el
    maybe (pure ()) (\x -> remove x [fromSel sel]) l

setHtml :: Element -> Html () -> IO ()
setHtml el html =
    setInnerHTML el $ Just (fromHtml html)

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

-- | console.log
foreign import javascript unsafe "console.log($1)" js_consoleLog :: JSString -> IO ()

consoleLog :: Text -> IO ()
consoleLog a = js_consoleLog (textToJSString a)

foreign import javascript unsafe "$1[\"value\"]" js_getValue :: Element -> IO JSString

getValue :: Element -> IO Text
getValue el = textFromJSString <$> js_getValue el

foreign import javascript unsafe "$1[\"value\"] = $2" js_setValue :: Element -> JSString -> IO ()

setValue :: Element -> Text -> IO ()
setValue el val = js_setValue el (textToJSString val)

foreign import javascript unsafe "$1.dataset.id" js_getDatasetId :: Element -> IO (Nullable Int)

itemId :: Element -> IO (Maybe Int)
itemId el = do
  nullableToMaybe <$> js_getDatasetId el

getItem :: NodeList -> Int -> IO (Maybe Node)
getItem list n =
  item list (fromIntegral n)

keyCode :: UIEvent -> IO Int
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
foreign import javascript unsafe "$1.addEventListener($2,$3,$4)" jsext_addEventListener :: Element -> JSString -> Callback (JSVal -> IO ()) -> Bool -> IO ()

on :: (IsEvent ev) => Element -> Text -> (Element -> E.EventM Element ev ()) -> IO (IO ()) 
on el uiAction handler = do
    rel <- E.on el (E.EventName uiAction) (handler el)
    pure rel
    
-- up to here
onWindow = undefined
delegate = undefined

foreign import javascript unsafe "$1.parentElement" jsParentElement :: Element -> IO (Element)
foreign import javascript unsafe "$1.querySelector($2)" jsElementQuerySelector :: Element -> JSString -> IO (Element)
foreign import javascript unsafe "$1.querySelectorAll($2)" jsQuerySelectorAll :: Element -> JSString -> IO (NodeList)
foreign import javascript unsafe "$1.tagName" jsTagName :: Element -> IO (Nullable JSString)
foreign import javascript unsafe "$1===$2" jsEq :: JSVal -> JSVal -> IO Bool
foreign import javascript unsafe "$1.length" jsLength :: NodeList -> IO Int
foreign import javascript unsafe "$1.checked" jsChecked :: Element -> IO (Bool)
foreign import javascript unsafe "$1.innerHTML = $2" jsSetHtml :: (Element) -> JSString -> IO ()
foreign import javascript unsafe "$1.item($2)" jsItem :: NodeList -> Word -> IO (Element)
foreign import javascript unsafe "$1.keyCode" jsKeyCode :: Event -> IO Int
foreign import javascript unsafe "$1.target" jsTarget :: Event -> IO (Element)
foreign import javascript unsafe "document.location.hash" jsGetHash :: IO (JSString)
foreign import javascript unsafe "document.querySelector($1)" jsQuerySelector :: JSString -> IO Element
foreign import javascript unsafe "window.addEventListener($1,$2,$3)" jsWindowAddEventListener :: JSString -> Callback (JSVal -> IO ()) -> JSVal -> IO ()
foreign import javascript unsafe "window.onload = $1" jsOnload :: Callback (IO ()) -> IO ()



-- * common idioms


{-

-- * manipulating dom

-- * event listening
on :: JSVal -> String -> (JSVal -> Event -> IO ()) -> IO () 
on el uiAction handler = do
  handler' <- sync1 (handler el)
  jsAddEventListener el (toJSString uiAction) handler' (toJSBool False)

onWindow :: String -> (JSVal -> Event -> IO ()) -> IO ()
onWindow uiAction handler = do
  handler' <- sync1 (handler jsNull)
  jsWindowAddEventListener
    (toJSString uiAction) 
    handler'
    (toJSBool False)

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
