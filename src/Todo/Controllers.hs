{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-type-defaults#-}

module Todo.Controllers where

import GHCJS.Extended (docElement, onWindow, on, delegate, Element(..), keyCode, itemId, getHash, elementOf, getValue, JSVal, toJSVal)
import MVC hiding ((<>))
import Todo.Model
import Protolude hiding (on)

data Keys = Escape | Enter | SomethingElse deriving (Eq)

toKeys :: Int -> Keys
toKeys n = case n of
  13 -> Enter
  27 -> Escape
  _  -> SomethingElse

sendAction :: a -> Output a -> IO ()
sendAction action o = void $ atomically $ send o action

controllers :: Output Action -> IO ()
controllers o = do
  toggleAll <- docElement ".toggle-all"
  clearCompleted <- docElement ".clear-completed"
  newTodo <- docElement ".new-todo"
  todos' <- docElement ".todo-list"
  onWindow "load" (const $ sendAction Refresh o)
  onWindow "hashchange" (const $ ctlHash o)
  on toggleAll "click" (const $ const $ sendAction ToggleAll o)
  on clearCompleted "click" (const $ const $ sendAction ClearCompleted o)
  on newTodo "keyup" (ctlNewItem o)
  delegate todos' ".destroy" "click" (\el _ -> ctlId DeleteItem o el)
  delegate todos' ".toggle" "change" (\el _ -> ctlId Toggle o el)
  delegate todos' "label" "dblclick" (\el _ -> ctlId EditItem o el)
  delegate todos' ".edit" "blur" (ctlEditItemDone o)
  delegate todos' ".edit" "keyup" (ctlEditKeyup o)

ctlId :: (ItemId -> Action) -> Output Action -> Element -> IO ()
ctlId action o el = do
    a_ <- idTagged action el
    sendAction a_ o

idTagged :: (ItemId -> Action) -> Element -> IO Action
idTagged action el = do
  id' <- itemId el
  case id' of
    Nothing -> pure NoAction
    Just x -> pure (action (ItemId x))

ctlEditKeyup :: Output Action -> Element -> JSVal -> IO ()
ctlEditKeyup o el ev = do
  code <- keyCode ev
  case toKeys code of
    Enter -> ctlEditItemDone o el ev
    Escape -> ctlId EditItemCancel o el
    _ -> pure ()

ctlEditItemDone :: Output Action -> Element -> JSVal -> IO ()
ctlEditItemDone o el _ = do
  box <- elementOf el ".edit"
  value <- getValue (Element box)
  a_ <- idTagged (\x -> EditItemDone x value) el
  sendAction a_ o
  
ctlHash :: Output Action -> IO ()
ctlHash o = do
  hash <- getHash
  let path = takeWhile (not . (== '/')) . drop 1 . dropWhile (not . (== '/')) $ (show hash)
  let path_ =
          case path of
                  "completed" -> Just Completed
                  "active"    -> Just Active
                  _           -> Nothing
  sendAction (Filter path_) o

ctlNewItem :: Output Action -> Element -> JSVal -> IO ()
ctlNewItem o el ev = do
  code <- keyCode ev
  value <- getValue el
  if toKeys code == Enter && value /= mempty
    then (sendAction (NewItem value) o)
    else pure ()
