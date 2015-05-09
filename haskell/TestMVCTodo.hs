{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module TestMVCTodo where

import           Control.Applicative
import           Control.Monad
import           Test.Tasty.Hspec
import MVC.Todo
import Data.Monoid
import Data.Default (def)
import Test.QuickCheck

demoActions :: [ToDoAction String]
demoActions = 
  mconcat ((\x -> [ EditItem ToDoNew, EditItemSave ToDoNew ("item number " ++ show x), Toggle ToDoNew]) <$> [0..10]) ++
  (Toggle . ToDoExisting <$> [2,4,6,8,10]) ++
  [ToggleAll] ++
  [ClearCompleted] ++
  (DeleteItem <$> [0,1]) ++
  mconcat ((\x -> [ EditItem (ToDoExisting x), EditItemSave (ToDoExisting x) ("item done: " ++ show x), Toggle . ToDoExisting $ x]) <$> [1,2])

demoTodos :: ToDos String
demoTodos = foldl (flip apply) def demoActions

demoAfterActions =
  ToDos
    { _todosNewText = ""
    , _todosItems =
        [ ToDoItem { _itemStatus = Active , _itemText = "item number 6" }
        , ToDoItem { _itemStatus = Completed , _itemText = "item done: 1" }
        , ToDoItem { _itemStatus = Completed , _itemText = "item done: 2" }
        ]
    }

tests :: IO (SpecWith())
tests =
  return $ describe "initial r&d" $
    it "canned demo ok"     $ 
      demoTodos `shouldBe` demoAfterActions

instance Arbitrary ToDoStatus where
  arbitrary = 
    elements 
    [ Active
    , Completed
    ]

instance Arbitrary ToDoIndex where
  arbitrary = do
    let maxI = 10
    frequency [ (2, pure ToDoNew)
              , (8, ToDoExisting <$> choose (0,maxI))
              ]

instance (Arbitrary a) => Arbitrary (ToDoItem a) where
  arbitrary = ToDoItem <$> arbitrary <*> arbitrary

instance (Arbitrary a) => Arbitrary (ToDos a) where
  arbitrary = ToDos <$> arbitrary <*> arbitrary

instance (Arbitrary a) => Arbitrary (ToDoAction a) where
  arbitrary = frequency
    [ (10, Toggle <$> arbitrary)
    , (2,  pure ToggleAll)
    , (6,  EditItem <$> arbitrary)
    , (6,  EditItemCancel <$> arbitrary)
    , (6,  EditItemSave <$> arbitrary <*> arbitrary)
    , (2,  Filter <$> arbitrary)
    , (2,  Focus <$> arbitrary)
    , (4,  DeleteItem <$> arbitrary)
    , (1,  pure ClearCompleted)
    ]

wow :: IO [ToDos String]
wow = 
  Prelude.zipWith apply <$> 
  sample' arbitrary <*> 
  sample' arbitrary

