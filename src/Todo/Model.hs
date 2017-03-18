{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC-fno-warn-type-defaults#-}

module Todo.Model 
  ( Todos(..)
  , HasTodos(..)
  , Action(..)
  , Out(..)
  , _StateOut
  , _ActionOut
  , Item(..)
  , itemText
  , itemStatus
  , HasItem(..)
  , ItemId(..)
  , ItemStatus(..)
  , toggleStatus
  , apply
  , model
  ) where

import           Control.Lens
-- import           Control.Monad
-- import           Control.Monad.State.Strict
import           Data.Default
import qualified Data.Map as Map
-- import           Data.Monoid
import           Pipes
import          Protolude

-- * ADTs
data ItemStatus 
  = Active
  | Completed
  deriving (Show, Eq)

toggleStatus :: ItemStatus -> ItemStatus
toggleStatus Active = Completed
toggleStatus Completed = Active

newtype ItemId = ItemId { unItemId :: Int }
  deriving (Show, Eq, Ord)

data Item = Item { _itemStatus :: ItemStatus, _itemText :: Text }
  deriving (Show, Eq)

data Todos =
  Todos
    { _todosNewItem :: Text                     -- drafted new todo item
    , _todosEditing :: Maybe ItemId             -- a todo item is being edited
    , _todosNextId  :: ItemId                   -- an item id source
    , _todosFilter  :: Maybe ItemStatus         -- maybe a filter is in place
    , _todosItems   :: Map.Map ItemId Item      -- the todo items
    }
  deriving (Show, Eq)

makeClassy ''Item

makeClassy ''Todos

instance Default Todos where
  def = Todos mempty Nothing (ItemId 0) Nothing Map.empty

data Action
  = ClearCompleted
  | DeleteItem ItemId
  | EditItem ItemId
  | EditItemCancel ItemId
  | EditItemDone ItemId Text
  | Filter (Maybe ItemStatus)
  | NewItem Text
  | NoAction
  | Refresh
  | Toggle ItemId
  | ToggleAll
  deriving (Show, Eq)

-- * algebra | apply an action to the todo model
apply :: Action -> Todos -> Todos
apply ClearCompleted tds =
  over todosItems (Map.filter (\x -> view itemStatus x /= Completed)) tds
apply (DeleteItem x) tds = over todosItems (Map.delete x) tds
apply (EditItem x) tds = set todosEditing (Just x) tds
apply (EditItemCancel _) tds = set todosEditing Nothing tds
apply (EditItemDone x t) tds = over todosItems adjustOrDelete $ set todosEditing Nothing tds
  where
    adjustOrDelete = 
      if t == mempty
        then Map.delete x
        else Map.adjust (set itemText t) x
apply (Filter f) tds = set todosFilter f tds
apply (NewItem t) tds =
  if t == mempty
    then tds
    else 
      over todosItems
      (Map.insert (view todosNextId tds) (Item Active t)) $ 
      over todosNextId (\(ItemId x) -> (ItemId (x + 1))) $ 
      set todosNewItem mempty tds
apply NoAction tds = tds
apply Refresh tds = tds
apply (Toggle x) tds =
  over todosItems (Map.adjust (over itemStatus toggleStatus) x) tds
apply ToggleAll tds =
  over todosItems (over (traverse . itemStatus) toggleStatus) tds

modifyState :: Action -> ListT (State Todos) Todos
modifyState action = case action of
  NoAction -> mzero
  _ -> do
    tds <- lift get
    let tds' = apply action tds
    lift $ put tds'
    pure tds'

data Out
  = StateOut Todos
  | ActionOut Action
  deriving (Show, Eq)

makePrisms ''Out

-- | apply the incoming action to state and pass through the action (just so it can be console logged)
model :: Action -> ListT (State Todos) Out
model action =
  (StateOut <$> modifyState action) -- <> (ActionOut <$> pure action)

