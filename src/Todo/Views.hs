{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC-fno-warn-type-defaults#-}

module Todo.Views 
  ( render
  ) where

import           Control.Lens hiding (element, elementOf, itemText, itemStatus)
import           Control.Monad (join)
import           Data.Foldable (sequenceA_)
import qualified Data.Map as Map
-- import           Data.Monoid
import           Data.Text (pack)
import           GHCJS.Extended (element, addClass, removeClass, Selector, setValue, setHtml, focusElement, elementOf, nullableToMaybe)
import           Lucid hiding (for_)
import           Todo.Model
import Protolude hiding (on, Selector)
import qualified Clay.Selector as Clay

render :: Todos -> IO ()
render = sequenceRenderers renderers

sequenceRenderers :: [Todos -> IO ()] -> Todos -> IO ()
sequenceRenderers rs tds = sequenceA_ $ ($) <$> rs <*> pure tds

renderers :: [Todos -> IO ()]
renderers =
  [ renderClearCompleted
  , renderFilter
  , renderHideStuff
  , renderNewItem
  , renderTodoCount
  , renderTodoList
  , renderFocus
  ]

renderClearCompleted :: Todos -> IO ()
renderClearCompleted tds = do
    el <- element ".clear-completed"
    maybe (pure ()) (\x -> action x hidden) el
  where
    hidden = "hidden" :: Selector
    numCompleted =
      getSum $ 
      foldMap
      (\x -> Sum (if view itemStatus x == Completed then 1 else 0))
      (view todosItems tds)
    action = if numCompleted == 0
               then addClass
               else removeClass

renderFilter :: Todos -> IO ()
renderFilter tds = do
  el <- element ".filters .selected"
  maybe (pure ()) (\x -> removeClass x selected) el
  -- FIXME: why does this break as a Selector
  newSelection <- element
      (Clay.element $ ".filters [href='#/" <> currentPage <> "']")
  maybe (pure ()) (\x -> addClass x selected) newSelection

  where
    selected = "selected" :: Selector
    currentPage =
      case view todosFilter tds of
        Nothing        -> ""
        Just Active    -> "active"
        Just Completed -> "completed"

renderHideStuff :: Todos -> IO ()
renderHideStuff tds = do
  main <- element ".main"
  foot <- element ".footer"
  maybe (pure ()) (\x -> action x hidden) main
  maybe (pure ()) (\x -> action x hidden) foot
  where
    hidden = "hidden" :: Selector
    action =
      if null (view todosItems tds)
        then addClass
        else removeClass

renderNewItem :: Todos -> IO ()
renderNewItem tds = do
    el <- element ".new-todo"
    maybe (pure ()) (\x -> setValue x (tds^.todosNewItem)) el

renderTodoCount :: Todos -> IO ()
renderTodoCount tds = do
    el <- element ".todo-count"
    maybe (pure ()) (\x -> setHtml x itemsLeft) el
  where
    itemsLeft = strong_ (toHtml $ (show n :: Text)) <> " item" <> s <> " left"
    n = getSum $
        foldMap
        (\x -> Sum (if view itemStatus x == Active then 1 else 0))
        (view todosItems tds) :: Integer
    s = if n == 1
          then ""
          else "s"

renderTodoList :: Todos -> IO ()
renderTodoList tds = do
    el <- element ".todo-list"
    maybe (pure ()) (\x -> setHtml x (htmlItems tds)) el

renderFocus :: Todos -> IO ()
renderFocus tds =
  case view todosEditing tds of
    Nothing -> pure ()
    Just _ -> do
      el <- element ".todo-list"
      editing <- case el of
        Nothing -> pure Nothing
        Just el' -> elementOf el' ".editing .edit"
      case editing of
        Nothing -> pure ()
        Just ed -> focusElement ed

htmlItems :: Todos -> Html ()
htmlItems tds =
  mconcat $ 
  fmap
  (\(ItemId id', item') ->
    htmlItem item' id' (editing == Just (ItemId id')) (visible item'))
  items
  where
    items = Map.toAscList (view todosItems tds)
    editing = view todosEditing tds
    visible item' =
      case view todosFilter tds of
        Nothing        -> True
        Just Active    -> view itemStatus item' == Active
        Just Completed -> view itemStatus item' == Completed

htmlItem :: Item -> Int -> Bool -> Bool -> Html ()
htmlItem item' id' editing visible =
  li_ (liClass <> [data_ "id" (pack $ show id')])
    (div_ itemClass 
     (input_ ([class_ "toggle", type_ "checkbox"] ++ checked)
      <> label_ (toHtml (view itemText item')) 
      <> button_ [class_ "destroy"] mempty)
     <> input_ [class_ "edit", value_ (view itemText item')])
  where
    liClass
      | editing && (view itemStatus item' == Completed) =
          [class_ "editing completed"]
      | editing = [class_ "editing"]
      | view itemStatus item' == Completed = [class_ "completed"]
      | otherwise = []
    itemClass =
      if visible
        then [class_ "view"]
        else [class_ "hidden"]
    checked =
      case view itemStatus item' of
        Completed -> [checked_]
        Active    -> []
