{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-type-defaults#-}

module Todo.Controllers where

import Control.Error.Extended (runEitherT, EitherT(..))
import GHCJS.Extended (UIEvent, element', onWindow, on, delegate, Element, Event, keyCode, itemId, getHash, elementOf, getValue)  
import MVC hiding ((<>))
import Todo.Model
import Protolude hiding (on)

data Keys = Escape | Enter | SomethingElse deriving (Eq)

toKeys :: Int -> Keys
toKeys n = case n of
  13 -> Enter
  27 -> Escape
  _  -> SomethingElse

send' :: Output a -> a -> IO ()
send' o action = void $ atomically $ send o action

controllers :: Output Action -> ExceptT Text IO ()
controllers o = do
  toggleAll <- element' ".toggle-all"
  clearCompleted <- element' ".clear-completed"
  newTodo <- element' ".new-todo"
  todos' <- element' ".todo-list"
  lift $ do
    onWindow "load" (ctl Refresh o)
    onWindow "hashchange" (ctlHash o)
    on toggleAll "click" (ctl ToggleAll o)
    on clearCompleted "click" (ctl ClearCompleted o)
    on newTodo "keyup" (ctlNewItem o)
    delegate todos' ".destroy" "click" (ctlId DeleteItem o)
    delegate todos' ".toggle" "change" (ctlId Toggle o)
    delegate todos' "label" "dblclick" (ctlId EditItem o)
    delegate todos' ".edit" "blur" (ctlEditItemDone o)
    delegate todos' ".edit" "keyup" (ctlEditKeyup o)

ctl :: Action -> Output Action -> Element -> Event -> IO ()
ctl a o _ _ = send' o a

ctlId :: (ItemId -> Action) -> Output Action -> Element -> UIEvent
    -> IO ()
ctlId action o el _ = send' o =<< idTagged action el

idTagged :: (ItemId -> Action) -> Element -> IO Action
idTagged action el = do
  id' <- itemId el
  case id' of
    Nothing -> pure NoAction
    Just x -> pure (action (ItemId x))

ctlEditKeyup :: Output Action -> Element -> UIEvent -> IO ()
ctlEditKeyup o el ev = do
  code <- keyCode ev
  case toKeys code of
    Enter -> ctlEditItemDone o el ev
    Escape -> ctlId EditItemCancel o el ev
    _ -> pure ()

ctlEditItemDone :: Output Action -> Element -> UIEvent -> IO ()
ctlEditItemDone o el _ = do
  box <- elementOf el ".edit"
  case box of
    Nothing -> pure ()
    Just box' -> do
        value <- getValue box'
        send' o =<< idTagged (\x -> EditItemDone x value) el

ctlHash :: Output Action -> Element -> Event -> IO ()
ctlHash o _ _ = do
  hash <- getHash
  case hash of
    Nothing -> pure ()
    Just hash' -> do
        let path = takeWhile (not . (== '/')) . drop 1 . dropWhile (not . (== '/')) $ (show hash')
        let path' =
                case path of
                  "completed" -> Just Completed
                  "active"    -> Just Active
                  _           -> Nothing
        send' o (Filter path')

ctlNewItem :: Output Action -> Element -> UIEvent -> IO ()
ctlNewItem o el ev = do
  code <- keyCode ev
  value <- getValue el
  if toKeys code == Enter && value /= mempty
    then send' o (NewItem value)
    else pure ()
