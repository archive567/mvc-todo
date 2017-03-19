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
import           GHCJS.Extended (Element(..), docElement, addClass, removeClass, Selector, setValue, setHtml, focus, elementOf)
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
    el <- docElement ".clear-completed"
    action el hidden
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
  el <- docElement ".filters .selected"
  removeClass el selected
  newSelection <- docElement
      (Clay.element $ ".filters [href='#/" <> currentPage <> "']")
  addClass newSelection selected
  where
    selected = "selected" :: Selector
    currentPage =
      case view todosFilter tds of
        Nothing        -> ""
        Just Active    -> "active"
        Just Completed -> "completed"

renderHideStuff :: Todos -> IO ()
renderHideStuff tds = do
  main <- docElement ".main"
  foot <- docElement ".footer"
  action main hidden
  action foot hidden
  where
    hidden = "hidden" :: Selector
    action =
      if null (view todosItems tds)
        then addClass
        else removeClass

renderNewItem :: Todos -> IO ()
renderNewItem tds = do
    el <- docElement ".new-todo"
    setValue el (tds^.todosNewItem)

renderTodoCount :: Todos -> IO ()
renderTodoCount tds = do
    el <- docElement ".todo-count"
    setHtml el itemsLeft
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
    el <- docElement ".todo-list"
    setHtml el (htmlItems tds)

renderFocus :: Todos -> IO ()
renderFocus tds =
  case view todosEditing tds of
    Nothing -> pure ()
    Just _ -> do
      el <- docElement ".todo-list"
      elEditing <- elementOf el ".editing .edit"
      focus (Element elEditing)

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
