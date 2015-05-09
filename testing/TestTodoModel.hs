{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module TestTodoModel where

import Todo.Model

import           Control.Applicative
import           Control.Monad
import           Test.Tasty.Hspec
import Data.Monoid
import Data.Default (def)
import Test.QuickCheck
import qualified Data.Map as Map

demoActions :: [Action String]
demoActions = 
  mconcat ((\x -> [ NewItem ("item number " ++ show x)]) <$> [0..5]) ++
  (Toggle . ItemId <$> [0,2,4]) ++
  [ToggleAll] ++
  [ClearCompleted] ++
  (DeleteItem . ItemId <$> [0]) ++
  mconcat ((\x -> [ EditItem (ItemId x), EditItemDone (ItemId x) ("item done: " ++ show x), Toggle (ItemId x)]) <$> [2])

demoTodos :: Todos String
demoTodos = foldl (flip apply) def demoActions

demoAfterActions :: Todos String
demoAfterActions =
  Todos "" Nothing (ItemId 6) Nothing
  ( Map.fromList
    [ (ItemId 2, Item { _itemStatus = Completed , _itemText = "item done: 2" })
    , (ItemId 4, Item { _itemStatus = Active    , _itemText = "item number 4" })
    ]
  )

tests :: IO (SpecWith())
tests =
  return $ describe "initial r&d" $
    it "canned demo ok"     $ 
      demoTodos `shouldBe` demoAfterActions

instance Arbitrary ItemStatus where
  arbitrary = 
    elements 
    [ Active
    , Completed
    ]

instance Arbitrary ItemId where
  arbitrary = do
    let maxI = 10
    ItemId <$> choose (0,maxI)

instance (Arbitrary a) => Arbitrary (Item a) where
  arbitrary = Item <$> arbitrary <*> arbitrary

instance (Arbitrary a) => Arbitrary (Todos a) where
  arbitrary = 
    Todos <$> 
    arbitrary <*> 
    frequency
    [ (8, pure Nothing)
    , (2, Just <$> arbitrary)
    ] <*>
    arbitrary <*>
    frequency
    [ (6, pure Nothing)
    , (4, Just <$> arbitrary)
    ] <*>
    (Map.fromList <$> (\x -> [x]) <$> ((,) <$> arbitrary <*> arbitrary))

instance (Arbitrary a) => Arbitrary (Action a) where
  arbitrary = frequency
    [ (10, Toggle <$> arbitrary)
    , (2,  pure ToggleAll)
    , (6,  NewItem <$> arbitrary)
    , (6,  EditItem <$> arbitrary)
    , (6,  EditItemCancel <$> arbitrary)
    , (6,  EditItemDone <$> arbitrary <*> arbitrary)
    , (2,  Filter <$> arbitrary)
    , (4,  DeleteItem <$> arbitrary)
    , (1,  pure ClearCompleted)
    ]

wow :: IO [Todos String]
wow = 
  Prelude.zipWith apply <$> 
  sample' arbitrary <*> 
  sample' arbitrary

