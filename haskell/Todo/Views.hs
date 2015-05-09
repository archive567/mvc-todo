{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# LANGUAGE OverloadedStrings #-}

module Todo.Views (
  render
  ) where

import Todo.Model
import GHCJS.Extended

import Lucid hiding (for_)
import Control.Lens hiding (element)
import Data.Monoid
import qualified Data.Map as Map
import Data.Text (pack)
import Data.Foldable (sequenceA_)
import Control.Monad (join)

render :: Todos String -> IO ()
render = sequenceRenderers renderers

sequenceRenderers :: [Todos String -> IO ()] -> Todos String -> IO ()
sequenceRenderers rs tds = sequenceA_ $ ($) <$> rs <*> pure tds

renderers :: [Todos String -> IO ()]
renderers = 
  [ renderClearCompleted
  , renderFilter
  , renderHideStuff
  , renderNewItem
  , renderTodoCount
  , renderTodoList
  , renderFocus
  ]

renderClearCompleted :: Todos a -> IO ()
renderClearCompleted tds = 
  join $ action <$> element ".clear-completed" <*> pure hidden
  where
    hidden = "hidden" :: Selector
    numCompleted = 
      getSum $ foldMap 
      (\x -> if view itemStatus x == Completed then Sum 1 else Sum 0) 
      (view todosItems tds)
    action = if numCompleted == 0 then addClass else removeClass

renderFilter :: Todos a -> IO ()
renderFilter tds = do
  el <- element ".filters .selected"
  removeClass el selected
  -- FIXME: why does this break as a Selector
  newSelection <- elementUnsafe (".filters [href='#/" <> currentPage <> "']")
  addClass newSelection selected
  where
    selected = "selected" :: Selector
    currentPage = case view todosFilter tds of
      Nothing -> ""
      Just Active -> "active"
      Just Completed -> "completed"

renderHideStuff :: Todos a -> IO ()
renderHideStuff tds = do 
  main <- element ".main"
  foot <- element ".footer"
  action main hidden
  action foot hidden
  where
    hidden = "hidden" :: Selector
    action =
      if length (view todosItems tds) == 0 
      then addClass 
      else removeClass

renderNewItem :: (ToJSString a) => Todos a -> IO ()
renderNewItem tds = 
  join $ 
  setValue <$> 
  element ".new-todo" <*> 
  pure (view todosNewItem tds) 

renderTodoCount :: Todos String -> IO ()
renderTodoCount tds = join $ setHtml <$> el <*> pure itemsLeft
  where
    el = element ".todo-count"
    itemsLeft = strong_ (toHtml $ show n) <> " item" <> s <> " left"
    n = getSum $ foldMap (\x -> if view itemStatus x == Active then Sum 1 else Sum 0) (view todosItems $ tds) :: Integer
    s = if n == 1 then "" else "s"

renderTodoList :: Todos String -> IO ()
renderTodoList tds = 
  join $
  setHtml <$>
  element ".todo-list" <*>
  pure (htmlItems tds)

renderFocus :: Todos a -> IO ()
renderFocus tds = do
  case view todosEditing tds of
    Nothing -> pure ()
    Just _ -> do
      el <- element ".todo-list"
      editing <- maybeJSNull <$> jsElementQuerySelector el ".editing .edit"
      case editing of
        Nothing -> pure ()
        Just ed -> jsFocus ed

htmlItems :: Todos String -> Html ()
htmlItems tds =
  mconcat $ fmap  
    (\(id',item) -> 
      htmlItem item id' (editing == Just id') (visible item)) items
  where
    items = Map.toAscList (view todosItems tds)
    editing = view todosEditing tds
    visible item' = case view todosFilter tds of
      Nothing        -> True
      Just Active    -> view itemStatus item' == Active
      Just Completed -> view itemStatus item' == Completed

htmlItem :: Item String -> Int -> Bool -> Bool -> Html ()
htmlItem item id' editing visible = 
  li_ (liClass <> [data_ "id" (pack $ show id')]) 
  (  div_ itemClass 
     (  input_ ([class_ "toggle", type_ "checkbox"] ++ checked) 
     <> label_ (toHtml (view itemText item)) <> button_ [class_ "destroy"] mempty
     ) 
  <> input_ [class_ "edit", value_ (pack $ view itemText item)]
  )
  where
  liClass 
    | editing && (view itemStatus item == Completed) 
      = [class_ "editing completed"]
    | editing = [class_ "editing"]
    | (view itemStatus item == Completed) = [class_ "completed"]
    | otherwise = [] 
  itemClass = 
    if visible
       then [class_ "view"]
       else [class_ "hidden"]
  checked = 
    case view itemStatus item of
      Completed -> [checked_]
      Active -> []



  



