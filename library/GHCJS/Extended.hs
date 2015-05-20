{-# LANGUAGE CPP #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}

module GHCJS.Extended 
  ( 
    maybeJSNull 
  , Element
  , Event
  , Selector
  , onload
  , element
  , element'
  , elementUnsafe
  , elementOf
  , elementsOf
  , on
  , onWindow
  , delegate
  , addClass
  , removeClass
  , setValue
  , checked
  , setHtml
  , itemId
  , focus
  , getValue
  , getHash
  , keyCode
  , consoleLog
  , sync
  , sync1
  , FromJSString(..)
  , ToJSString(..)
  , JSRef
  ) where

#ifdef __GHCJS__
import GHCJS.Types
import GHCJS.Marshal
import GHCJS.DOM.Types (maybeJSNull, Element, Event, NodeList)
import GHCJS.Foreign
#endif

import           Clay (Selector, renderSelector)
import           Control.Error.Extended
import           Data.Char
import           Data.Foldable
import           Data.Monoid
import qualified Data.Text.Lazy as Text
import           Lucid

#ifndef __GHCJS__
data Event
type JSObject a = JSRef (JSObject_ a)
data JSObject_ a
data JSRef a
data JSFun a
data JSBool a
-- | A DOM element.
data Element

data NodeList
castRef = undefined
data JSString
class ToJSString a where 
  toJSString :: a -> JSString
  toJSString = undefined
instance ToJSString String
instance FromJSString String
class FromJSString a where 
  fromJSString :: JSString -> a
  fromJSString = undefined
newObj = undefined
setProp = undefined
getProp = undefined
jsNull = undefined
maybeJSNull = undefined
toJSBool = undefined
toJSRef_aeson = undefined
syncCallback1 = undefined
syncCallback2 = undefined
syncCallback = undefined
data AlwaysRetain = AlwaysRetain
class ToJSRef a where
  toJSRef :: a -> IO (JSRef a)
  toJSRef = undefined
  toJSRefListOf :: [a] -> IO (JSRef [a])
  toJSRefListOf = undefined
class FromJSRef a where
  fromJSRef :: JSRef a -> IO (Maybe a)
  fromJSRef = undefined
  fromJSRefListOf :: JSRef [a] -> IO (Maybe [a])
  fromJSRefListOf = undefined
instance FromJSRef Int
instance FromJSRef JSString
instance FromJSRef Element
instance FromJSRef x => FromJSRef [x]
instance ToJSRef x => ToJSRef [x]
instance ToJSRef x => ToJSRef (JSRef x)
instance ToJSRef Int
#endif


#ifdef __GHCJS__
foreign import javascript unsafe "$1.addEventListener($2,$3,$4)" jsAddEventListener :: JSRef Element -> JSString -> JSFun (JSRef Event -> IO ()) -> JSBool -> IO ()
foreign import javascript unsafe "$1.classList.add($2)" jsAddClass :: (JSRef Element) -> JSString -> IO ()
foreign import javascript unsafe "$1.classList.remove($2)" jsRemoveClass :: (JSRef Element) -> JSString -> IO ()
foreign import javascript unsafe "$1.dataset.id" jsId :: JSRef Element -> IO JSString
foreign import javascript unsafe "$1.focus()" jsFocus :: JSRef Element -> IO ()
foreign import javascript unsafe "$1.parentElement" jsParentElement :: JSRef Element -> IO (JSRef Element)
foreign import javascript unsafe "$1.querySelector($2)" jsElementQuerySelector :: JSRef Element -> JSString -> IO (JSRef Element)
foreign import javascript unsafe "$1.querySelectorAll($2)" jsQuerySelectorAll :: JSRef Element -> JSString -> IO (JSRef NodeList)
foreign import javascript unsafe "$1.tagName" jsTagName :: JSRef Element -> IO (JSString)
foreign import javascript unsafe "$1===$2" jsEq :: JSRef a -> JSRef a -> IO Bool
foreign import javascript unsafe "$1.length" jsLength :: JSRef NodeList -> IO Int
foreign import javascript unsafe "$1.value" jsGetValue :: JSRef Element -> IO JSString
foreign import javascript unsafe "$1.value = $2" jsSetValue :: JSRef Element -> JSString -> IO ()
foreign import javascript unsafe "$1.checked" jsChecked :: JSRef Element -> IO (JSBool)
foreign import javascript unsafe "$1.innerHTML = $2" jsSetHtml :: (JSRef Element) -> JSString -> IO ()
foreign import javascript unsafe "$1.item($2)" jsItem :: JSRef NodeList -> Word -> IO (JSRef Element)
foreign import javascript unsafe "$1.keyCode" jsKeyCode :: JSRef Event -> IO Int
foreign import javascript unsafe "$1.target" jsTarget :: JSRef Event -> IO (JSRef Element)
foreign import javascript unsafe "console.log($1)" jsConsoleLog :: JSString -> IO ()
foreign import javascript unsafe "document.location.hash" jsGetHash :: IO (JSString)
foreign import javascript unsafe "document.querySelector($1)" jsQuerySelector :: JSString -> IO (JSRef Element)
foreign import javascript unsafe "window.addEventListener($1,$2,$3)" jsWindowAddEventListener :: JSString -> JSFun (JSRef Event -> IO ()) -> JSBool -> IO ()
foreign import javascript unsafe "window.onload = $1" jsOnload :: JSFun a -> IO ()
#endif

#ifndef __GHCJS__
jsAddEventListener = undefined
jsAddClass = undefined
jsRemoveClass = undefined
jsId = undefined
jsFocus = undefined
jsParentElement = undefined
jsElementQuerySelector = undefined
jsQuerySelectorAll = undefined
jsTagName = undefined
jsEq = undefined
jsLength = undefined
jsGetValue = undefined
jsSetValue = undefined
jsSetHtml = undefined
jsItem = undefined
jsKeyCode = undefined
jsTarget = undefined
jsConsoleLog = undefined
jsGetHash = undefined
jsQuerySelector = undefined
jsWindowAddEventListener = undefined
jsOnload = undefined
#endif

-- * orphans
instance ToJSString (Html a) where
  toJSString = toJSString . Text.unpack . renderText

instance ToJSString (Selector) where
  toJSString = toJSString . renderSelector

unpack :: Selector -> String
unpack = Text.unpack . renderSelector

{-
instance ToJSString Text.Text where
  toJSString = toJSString . Text.unpack

instance FromJSString Text.Text where
  fromJSString = Text.pack . fromJSString
-}

-- * common syncing patterns for haskell functions
sync1 :: (JSRef a -> IO b) -> IO (JSFun (JSRef a -> IO b))
sync1 f = syncCallback1 AlwaysRetain True f

sync :: IO a -> IO (JSFun (IO a))
sync f = syncCallback AlwaysRetain True f

-- * common idioms

-- | console.log
consoleLog :: (ToJSString a) => a -> IO ()
consoleLog a = jsConsoleLog (toJSString a)

-- | onload
onload :: IO () -> IO ()
onload f = jsOnload =<< sync f

-- * element selection
element :: Selector -> IO (JSRef Element)
element sel = jsQuerySelector (toJSString sel)

element' :: Selector -> EitherT String IO (JSRef Element) 
element' sel = do
  sel'  <- lift $ jsQuerySelector (toJSString sel)
  failWith ("element selector " <> show sel <> " failed") (maybeJSNull sel')

elementUnsafe :: String -> IO (JSRef Element)
elementUnsafe sel = jsQuerySelector (toJSString sel)

elementOf :: JSRef Element -> Selector -> IO (JSRef Element) 
elementOf el sel = jsElementQuerySelector el (toJSString sel)

elementsOf :: JSRef Element -> Selector -> IO (JSRef NodeList) 
elementsOf el sel = jsQuerySelectorAll el (toJSString sel)

-- * manipulating dom
setHtml :: JSRef Element -> Html a -> IO ()
setHtml el html = jsSetHtml el (toJSString html)

addClass :: (ToJSString cl) => JSRef Element -> cl -> IO ()
addClass el sel = jsAddClass el (toJSString sel) 

removeClass :: (ToJSString cl) => JSRef Element -> cl -> IO ()
removeClass el sel = jsRemoveClass el (toJSString sel) 

setValue :: (ToJSString a) => JSRef Element -> a -> IO ()
setValue el val = jsSetValue el (toJSString val)

getValue :: (FromJSString a) => JSRef Element -> IO a
getValue el = fromJSString <$> jsGetValue el

checked :: JSRef Element -> IO Bool
checked el = fromJSBool <$> jsChecked el

-- * event listening
on :: JSRef Element -> String -> (JSRef Element -> JSRef Event -> IO ()) -> IO () 
on el uiAction handler = do
  handler' <- sync1 (handler el)
  jsAddEventListener el (toJSString uiAction) handler' (toJSBool False)

onWindow :: String -> (JSRef Element -> JSRef Event -> IO ()) -> IO ()
onWindow uiAction handler = do
  handler' <- sync1 (handler jsNull)
  jsWindowAddEventListener
    (toJSString uiAction) 
    handler'
    (toJSBool False)

delegate :: JSRef Element -> Selector -> String -> (JSRef Element -> JSRef Event -> IO ()) -> IO ()
delegate base pattern ev action = do
  let useCapture = toJSBool $ ev == "blur" || ev == "focus"
  dispatch <- sync1 $ applyIn base pattern action 
  jsAddEventListener base (toJSString ev) dispatch useCapture

applyIn :: JSRef Element -> Selector -> (JSRef Element -> JSRef Event -> IO ()) -> JSRef Event -> IO ()
applyIn base pattern action ev = do
  t <- jsTarget ev
  p <- jsQuerySelectorAll base (toJSString pattern)
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

focus :: JSRef Element -> IO ()
focus = jsFocus

itemId :: JSRef Element -> EitherT String IO Int
itemId el = do
  i <- lift $ jsId (castRef el)
  tryRead "Int conversion failed" (fromJSString i)

getHash :: (FromJSString a) => IO a
getHash = do
  hash <- jsGetHash
  -- consoleLog hash
  pure $ fromJSString hash

-- | go up the tree and find an element, if it exists
findUp :: JSRef Element -> Selector -> IO (Maybe (JSRef Element))
findUp base sel = do
  parent <- maybeJSNull <$> jsParentElement base
  case parent of
    Nothing -> pure Nothing
    Just parent' -> do
      name <- jsTagName parent'
      if (toLower <$> (fromJSString name)) == (toLower <$> (unpack sel))
        then (pure $ Just parent')
        else (findUp parent' sel)

findM :: (Monad m, Traversable t) => (a -> m Bool) -> t a -> m (Maybe a)
findM p = fmap (getFirst . fold) . mapM 
       (fmap First . (\x -> do
           p' <- p x
           pure $ if p' then Just x else Nothing))

getItem :: JSRef NodeList -> Int -> IO (Maybe (JSRef Element))
getItem list n = do
  item <- jsItem list (fromIntegral n)
  item' <- fromJSRef item
  case item' of
    Nothing -> return Nothing
    Just _ -> return (Just item)

keyCode :: JSRef Event -> IO Int
keyCode ev = jsKeyCode ev
