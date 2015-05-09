{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# LANGUAGE FlexibleContexts #-}

module Todo.Model where

import Prelude hiding (foldl)
import Control.Monad
import Control.Lens
import Data.Default
import qualified Data.Map as Map
import Control.Monad.State.Strict
import Pipes

-- * ADTs

data ItemStatus = Active | Completed deriving (Show, Eq)

toggleStatus :: ItemStatus -> ItemStatus
toggleStatus Active = Completed
toggleStatus Completed = Active

data Id = Int deriving (Show, Eq, Ord)

data Item a = Item 
  { _itemStatus :: ItemStatus
  , _itemText :: a 
  } deriving (Show, Eq)

data Todos a = Todos 
  { _todosNewItem :: a
  , _todosEditing :: Maybe Int
  , _todosNextId :: Int
  , _todosFilter :: Maybe ItemStatus
  , _todosItems :: Map.Map Int (Item a)
  } deriving (Show, Eq)

makeLenses ''Item
makeLenses ''Todos

instance (Monoid a) => Default (Todos a) where
  def = Todos mempty Nothing 0 Nothing Map.empty

data Action a 
  = ClearCompleted
  | DeleteItem Int
  | EditItem Int
  | EditItemCancel Int
  | EditItemDone Int a
  | Filter (Maybe ItemStatus)
  | Editing (Maybe Int)
  | NewItem a
  | NoAction
  | Refresh
  | Toggle Int
  | ToggleAll
  deriving (Show, Eq)

-- * algebra
-- | apply an action to the todo model
apply :: (Eq a, Monoid a) => Action a -> Todos a -> Todos a
apply ClearCompleted tds = 
  over todosItems (Map.filter (\x -> view itemStatus x /= Completed)) tds
apply (DeleteItem x) tds = over todosItems (Map.delete x) tds
apply (EditItem x) tds = set todosEditing (Just x) $ tds 
apply (EditItemCancel _) tds = set todosEditing Nothing $ tds 
apply (EditItemDone x t) tds = add $ set todosEditing Nothing $ tds
  where
  add = 
    if t == mempty
    then over todosItems (Map.delete x)
    else over todosItems (Map.adjust (set itemText t) x)

apply (Filter f) tds = set todosFilter f tds
apply (Editing f) tds = set todosEditing f tds
apply (NewItem t) tds = 
  if t == mempty
  then tds
  else 
    ( over todosItems (Map.insert (view todosNextId tds) (Item Active t))
    $ over todosNextId (+1)
    $ set todosNewItem mempty 
    $ tds
    )
apply NoAction tds = tds
apply Refresh tds = tds
apply (Toggle x) tds =
  over todosItems (Map.adjust (over itemStatus toggleStatus) x) tds
apply ToggleAll tds =
  over todosItems (over (traverse . itemStatus) toggleStatus) tds

model :: Pipe (Action String) (Todos String) (State (Todos String)) ()
model = forever $ do
  action <- await
  tds <- lift get
  let tds' = apply action tds
  lift $ put tds' 
  yield tds'

data Out 
  = StateOut (Todos String) 
  | ActionOut (Action String)

makePrisms ''Out

model' :: Pipe (Action String) Out (State (Todos String)) ()
model' = forever $ do
  action <- await
  tds <- lift get
  let tds' = apply action tds
  lift $ put tds'
  yield $ ActionOut action
  yield $ StateOut tds'




