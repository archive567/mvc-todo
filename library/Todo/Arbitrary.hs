{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Todo.Arbitrary where

import Todo.Model

import           Control.Applicative
import           Control.Monad
import Data.Monoid
import Data.Default (def)
import Test.QuickCheck
import qualified Data.Map as Map
import Data.String
import Data.List (nub)

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

instance Arbitrary (Item String) where
  arbitrary = Item <$> arbitrary <*> (show <$> (arbitrary :: Gen TodoStatement))

instance Arbitrary (Todos String) where
  arbitrary = 
    Todos <$> 
    (show <$> (arbitrary :: Gen HaskellVerb)) <*> 
    frequency
    [ (8, pure Nothing)
    , (2, Just <$> arbitrary)
    ] <*>
    arbitrary <*>
    frequency
    [ (6, pure Nothing)
    , (4, Just <$> arbitrary)
    ] <*>
    (Map.fromList <$> (: []) <$> ((,) <$> arbitrary <*> arbitrary))

instance Arbitrary (Action String) where
  arbitrary = frequency
    [ (10, Toggle <$> arbitrary)
    , (2,  pure ToggleAll)
    , (6,  NewItem <$> (show <$> (arbitrary :: Gen TodoStatement)))
    , (6,  EditItem <$> arbitrary)
    , (6,  EditItemCancel <$> arbitrary)
    , (6,  EditItemDone <$> arbitrary <*> (show <$> (arbitrary :: Gen TodoStatement)))
    , (2,  Filter <$> arbitrary)
    , (4,  DeleteItem <$> arbitrary)
    , (1,  pure ClearCompleted)
    ]

testApply :: IO [Todos String]
testApply = 
  Prelude.zipWith apply <$> 
  sample' arbitrary <*> 
  sample' arbitrary

data TodoStatement = TodoStatement HaskellVerb HaskellNoun

instance Show TodoStatement where
  show (TodoStatement verb noun) = show verb <> " " <> show noun

newtype HaskellVerb = HaskellVerb { unVerb :: String }

instance IsString HaskellVerb where
  fromString = HaskellVerb

instance Show HaskellVerb where
  show (HaskellVerb s) = s

newtype HaskellPrefix = HaskellPrefix { unPrefix :: String } deriving (Show, Eq)
newtype Haskellism    = Haskellism    { unHaskellism :: String } deriving (Show, Eq)
newtype HaskellSuffix = HaskellSuffix { unSuffix :: String } deriving (Show, Eq)

data HaskellNoun = HaskellNoun [HaskellPrefix] Haskellism [HaskellSuffix]

instance Show HaskellNoun where
  show (HaskellNoun ps h ss) = mconcat (unPrefix <$> ps) <> unHaskellism h <> mconcat (unSuffix <$> ss)

instance IsString HaskellNoun where
  fromString s = HaskellNoun [] (Haskellism s) []

instance IsString Haskellism where
  fromString = Haskellism

instance Arbitrary (TodoStatement) where
  arbitrary = TodoStatement <$> arbitrary <*> arbitrary

instance Arbitrary (HaskellNoun) where
  arbitrary = frequency $ 
    [ (20, HaskellNoun <$> ((take 2 . nub) <$> arbitrary) <*> arbitrary <*> ((take 1) <$> arbitrary)) 
    , (1, pure "cabal hell")
    , (1, pure "ADTs")
    , (1, pure "everything")
    , (5, HaskellNoun <$> pure [] <*> arbitrary <*> pure [])
    ]

instance Arbitrary (HaskellVerb) where
  arbitrary = frequency $ (\(x,y) -> (x, pure y)) <$>
    [ (3, "invent")
    , (3, "ponder")
    , (5, "code")
    , (1, "beta-reduce")
    , (1, "lambdify")
    , (3, "refactor")
    , (2, "reduce")
    , (1, "DeBruijnize")
    , (2, "curry")
    , (1, "howard-curry")
    , (1, "simplify")
    , (1, "complexificate")
    , (2, "git the")
    , (1, "build")
    , (1, "prettify")
    , (1, "compile")
    , (1, "generalize")
    , (1, "abstract")
    , (1, "ignore")
    , (1, "saturate")
    -- , (3, show <$> (arbitrary :: Gen Haskellism))
    ]

instance Arbitrary (HaskellPrefix) where
  arbitrary = frequency $ (\(x,y) -> (x, pure (HaskellPrefix y))) <$>
    [ (1, "homo-")
    , (1, "functo-")
    , (2, "contra-")
    , (2, "bi-")
    , (3, "iso-")
    , (2, "pro-")
    , (4, "co-")
    , (4, "free-")
    , (1, "endo-")
    , (1, "morphic-")
    , (10, "")
    ]

instance Arbitrary (HaskellSuffix) where
  arbitrary = frequency $ (\(x,y) -> (x, pure (HaskellSuffix y))) <$>
    [ (1, "-ism")
    , (1, "-orial")
    , (1, "-ic")
    , (12, "")
    ]

instance Arbitrary (Haskellism) where
  arbitrary = frequency $ (\(x,y) -> (x, pure (Haskellism y))) <$>
    [ (6, "functor")
    , (4, "monoid")
    , (1, "dimap")
    , (3, "applicative")
    , (2, "arrow")
    , (3, "monad")
    , (1, "something")
    ]







