{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}

module GHCJS.Extended 
  ( module GHCJS.Extended 
  , module GHCJS.Foreign
  , module GHCJS.Types 
  , module GHCJS.Marshal 
  , module GHCJS.DOM.Types
  , module GHCJS.DOM.Element
  , Selector
  ) where


import GHCJS.Foreign
import GHCJS.Marshal
import GHCJS.Types
import GHCJS.DOM.Types
import GHCJS.DOM
import GHCJS.DOM.Element
import GHCJS.DOM.HTMLElement
import GHCJS.DOM.HTMLInputElement
import GHCJS.DOM.EventM
import Control.Error.Extended
import Control.Monad (join)
import Control.Monad.Trans
import Lucid
import qualified Data.Text.Lazy as Text
import Data.Monoid
import Data.Char
import Data.Foldable

import Clay (Selector, renderSelector)

instance ToJSString (Html a) where
  toJSString = toJSString . Text.unpack . renderText

instance ToJSString (Selector) where
  toJSString = toJSString . renderSelector

unpack :: Selector -> String
unpack = Text.unpack . renderSelector

instance ToJSString Text.Text where
  toJSString = toJSString . Text.unpack

instance FromJSString Text.Text where
  fromJSString = Text.pack . fromJSString

-- * common sync'ing
sync1 :: (JSRef a -> IO b) -> IO (JSFun (JSRef a -> IO b))
sync1 f = syncCallback1 AlwaysRetain True f

sync :: IO a -> IO (JSFun (IO a))
sync f = syncCallback AlwaysRetain True f


-- * common idioms
foreign import javascript unsafe "console.log($1)" jsConsoleLog :: JSString -> IO ()

-- | onload
foreign import javascript unsafe "window.onload = $1" jsOnload :: JSFun a -> IO ()

onload :: IO () -> IO ()
onload f = jsOnload =<< syncCallback AlwaysRetain True f

-- | FIXME: this seems to be the GHCJS.DOM way of doing events eg http://stackoverflow.com/questions/25921144/ghcjs-dom-event-guidance, but I couldn't get it to work.
-- main = onload' (print "onload'")
onload' :: IO () -> IO ()
onload' f = do
  Just window <- currentWindow
  let c = connect ("load" :: String) window :: EventM Event DOMWindow () -> IO (IO ())
  join $ c $ liftIO f

-- * querySelector
foreign import javascript unsafe "document.querySelector($1)" jsQuerySelector :: JSString -> IO (JSRef Element)

element :: Selector -> IO (JSRef Element)
element sel = jsQuerySelector (toJSString sel)

element' :: Selector -> EitherT String IO (JSRef Element) 
element' sel = do
  sel'  <- lift $ jsQuerySelector (toJSString sel)
  failWith ("element selector " <> show sel <> " failed") (maybeJSNull sel')

elementUnsafe :: String -> IO (JSRef Element)
elementUnsafe sel = jsQuerySelector (toJSString sel)

foreign import javascript unsafe "$1.querySelector($2)" 
  jsElementQuerySelector :: JSRef Element -> JSString -> IO (JSRef Element)

foreign import javascript unsafe "$1.focus()" 
  jsFocus :: JSRef Element -> IO ()

-- * manipulating dom html
foreign import javascript unsafe "$1[\"innerHTML\"] = $2" jsSetHtml :: (JSRef Element) -> JSString -> IO ()

setHtml :: JSRef Element -> Html a -> IO ()
setHtml el html = jsSetHtml el (toJSString html)

setHtml' :: Element -> Html a -> IO ()
setHtml' el html = htmlElementSetInnerHTML (castToHTMLElement el) html

-- * class effects
foreign import javascript unsafe "$1.classList.add($2)" jsAddClass :: (JSRef Element) -> JSString -> IO ()

addClass :: (ToJSString cl) => JSRef Element -> cl -> IO ()
addClass el sel = jsAddClass el (toJSString sel) 

addClass' :: (ToJSString cl) => Element -> cl -> IO ()
addClass' el sel = 
  join $ jsAddClass <$> toJSRef el <*> pure (toJSString sel) 

foreign import javascript unsafe "$1.classList.remove($2)" jsRemoveClass :: (JSRef Element) -> JSString -> IO ()

removeClass :: (ToJSString cl) => JSRef Element -> cl -> IO ()
removeClass el sel = jsRemoveClass el (toJSString sel) 

removeClass' :: (ToJSString cl) => Element -> cl -> IO ()
removeClass' el sel = 
  join $ jsRemoveClass <$> toJSRef el <*> pure (toJSString sel) 

-- * set value
foreign import javascript unsafe "$1['value']=$2" jsSetValue :: JSRef Element -> JSString -> IO ()

setValue :: (ToJSString a) => JSRef Element -> a -> IO ()
setValue el val = jsSetValue el (toJSString val)

setValue' :: (ToJSString a) => Element -> a -> IO ()
setValue' el val =
  htmlInputElementSetValueForUser (castToHTMLInputElement el) val

foreign import javascript unsafe "$1['value']" jsGetValue :: JSRef Element -> IO JSString

-- * miscellaneous

foreign import javascript unsafe "$1.dataset.id" jsId :: JSRef Node -> IO JSString

itemId :: JSRef Element -> EitherT String IO Int
itemId el = do
  i <- lift $ jsId (castRef el)
  tryRead "Int conversion failed" (fromJSString i)

foreign import javascript unsafe "document.location.hash" jsGetHash :: IO (JSString)

foreign import javascript unsafe "$1.parentElement" jsParentElement :: JSRef Element -> IO (JSRef Element)

foreign import javascript unsafe "$1.tagName" jsTagName :: JSRef Element -> IO (JSString)

foreign import javascript unsafe "$1===$2" jsEq :: JSRef a -> JSRef a -> IO Bool

-- * listeners for events

foreign import javascript unsafe "$1.addEventListener($2,$3,$4)" 
  jsAddEventListener :: JSRef Element -> JSString -> JSFun (JSRef Event -> IO ()) -> JSBool -> IO ()

on :: JSRef Element -> String -> (JSRef Element -> JSRef Event -> IO ()) -> IO () 
on el uiAction handler = do
  handler' <- sync1 (handler el)
  jsAddEventListener el (toJSString uiAction) handler' (toJSBool False)

foreign import javascript unsafe "window.addEventListener($1,$2,$3)" 
  jsWindowAddEventListener :: JSString -> JSFun (JSRef Event -> IO ()) -> JSBool -> IO ()

onWindow :: String -> (JSRef Element -> JSRef Event -> IO ()) -> IO ()
onWindow uiAction handler = do
  handler' <- sync1 (handler jsNull)
  jsWindowAddEventListener
    (toJSString uiAction) 
    handler'
    (toJSBool False)

-- | go up the tree and find the selector from the element, if it exists
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

foreign import javascript unsafe "$1['length']"
        jsLength :: JSRef NodeList -> IO Int

foreign import javascript unsafe "$1[\"item\"]($2)"
        jsItem ::
        JSRef NodeList -> Word -> IO (JSRef Element)

getItem :: JSRef NodeList -> Int -> IO (Maybe (JSRef Element))
getItem list n = do
  item <- jsItem list (fromIntegral n)
  item' <- fromJSRef item
  case item' of
    Nothing -> return Nothing
    Just _ -> return (Just item)

foreign import javascript unsafe "$1[\"target\"]"
        jsTarget :: JSRef Event -> IO (JSRef Element)

foreign import javascript unsafe "$1[\"querySelectorAll\"]($2)"
        jsQuerySelectorAll ::
        JSRef Element -> JSString -> IO (JSRef NodeList)

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

delegate :: JSRef Element -> Selector -> String -> (JSRef Element -> JSRef Event -> IO ()) -> IO ()
delegate base pattern ev action = do
  let useCapture = toJSBool $ ev == "blur" || ev == "focus"
  dispatch <- sync1 $ applyIn base pattern action 
  jsAddEventListener base (toJSString ev) dispatch useCapture

foreign import javascript unsafe "$1[\"keyCode\"]"
        jsKeyCode :: JSRef Event -> IO Int
