{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-type-defaults#-}

module Todo.Controllers where

import Control.Error.Extended (runEitherT, EitherT(..))
import GHCJS.Extended (FromJSString(..), element', onWindow, on, delegate, JSRef, Element, Event, keyCode, itemId, getHash, elementOf, getValue)  
import MVC
import Todo.Model

data Keys = Escape | Enter | SomethingElse deriving (Eq)

toKeys :: Int -> Keys
toKeys n = case n of
  13 -> Enter
  27 -> Escape
  _  -> SomethingElse

send' :: Output a -> a -> IO ()
send' o action = void $ atomically $ send o action

controllers :: Output (Action String) -> EitherT String IO ()
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

ctl :: Action a -> Output (Action a) -> JSRef Element -> JSRef Event -> IO ()
ctl a o _ _ = send' o a

ctl' :: (Show a) => Action a -> Output (Action a) -> JSRef Element -> JSRef Event -> IO ()
ctl' a o _ _ = do
  print $ "hi" <> show a
  send' o a

ctlId :: (Eq a, FromJSString a, Monoid a) => (ItemId -> Action a) -> Output (Action a) -> JSRef Element -> JSRef Event -> IO ()
ctlId action o el _ = send' o =<< idTagged action el

idTagged :: (ItemId -> Action a) -> JSRef Element -> IO (Action a)
idTagged action el = do
  id' <- runEitherT (itemId el)
  case id' of
    Left e  -> print e >> return NoAction
    Right x -> return (action (ItemId x))

ctlEditKeyup :: (Monoid a, Eq a, FromJSString a) => Output (Action a) -> JSRef Element -> JSRef Event -> IO ()
ctlEditKeyup o el ev = do
  code <- keyCode ev
  case toKeys code of
    Enter -> ctlEditItemDone o el ev
    Escape -> ctlId EditItemCancel o el ev
    _ -> pure ()

ctlEditItemDone :: (FromJSString a) => Output (Action a) -> JSRef Element -> JSRef Event -> IO ()
ctlEditItemDone o el _ = do
  box <- elementOf el ".edit"
  value <- getValue box
  send' o =<< idTagged (\x -> EditItemDone x (fromJSString value)) el

ctlHash :: Output (Action a) -> JSRef Element -> JSRef Event -> IO ()
ctlHash o _ _ = do
  hash <- getHash
  let path = takeWhile (not . (== '/')) . drop 1 . dropWhile (not . (== '/')) $ hash
  let path' =
        case path of
          "completed" -> Just Completed
          "active"    -> Just Active
          _           -> Nothing
  send' o (Filter path')

ctlNewItem :: (Eq a, FromJSString a, Monoid a) => Output (Action a) -> JSRef Element -> JSRef Event -> IO ()
ctlNewItem o el ev = do
  code <- keyCode ev
  value <- getValue el
  if toKeys code == Enter && value /= mempty
    then send' o (NewItem value)
    else pure ()
