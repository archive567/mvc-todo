{-# LANGUAGE RankNTypes #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import           Control.Concurrent (threadDelay)
import           Control.Error.Extended (runEitherPrintError)
import           Control.Lens hiding (element)
import           Control.Monad.State.Strict
import           Data.IORef
import qualified Data.Map as Map
import           GHCJS.Extended
import           MVC
import           MVC.Prelude as MVC
import           Test.QuickCheck
import           Todo.Arbitrary ()
import           Todo.Controllers (controllers)
import           Todo.Model (Todos(..), Action(..), ItemStatus(..), Item(..), ItemId(..), apply)
import           Todo.Views (render)

data Auto = Manual
          | Automatic
  deriving (Show, Eq)

toggle :: Auto -> Auto
toggle Manual = Automatic
toggle Automatic = Manual

data In = AutoIn Auto
        | ActionIn (Action String)
  deriving (Eq, Show)

data Out
  = TodosOut (Todos String)
  | InOut In
  | AutoOut Auto
  deriving (Show, Eq)

data State' = State' { _sTodos :: Todos String, _sAuto :: Auto } deriving (Show, Eq)

makeClassy  ''State'

makePrisms ''In
makePrisms ''Out

initialState :: State'
initialState = State'
  (Todos "" Nothing (ItemId 3) Nothing
    (Map.fromList $
       zip (ItemId <$> [0 ..]) 
       [ Item Completed "learn functional programming"
       , Item Completed "invent ADTs"
       , Item Completed "discover algebra between ADTs"
       , Item Active "automate everything else"
       ]))
  Manual

ctlAuto :: Output Auto -> IO ()
ctlAuto o = do
  toggleAuto <- element ".toggle-auto"
  on toggleAuto "click" (\el _ -> do
                            ch <- checked el
                            -- print ch
                            let auto = if ch then Automatic else Manual
                            void $ atomically $ send o auto)

modifyState :: In -> ListT (State State') Out
modifyState i = case i of
  (AutoIn auto) -> lift (modify (set sAuto auto)) >> pure (AutoOut auto)
  (ActionIn NoAction) -> mzero
  (ActionIn action) -> do
    s <- lift get
    let tds = view sTodos s
    let tds' = apply action tds
    lift $ put $ set sTodos tds' s
    pure (TodosOut tds')

model' :: In -> ListT (State State') Out
model' i =
  (modifyState i) <> (InOut <$> pure i)

run :: IO State'
run = do
  print "running auto ..."
  (o, i) <- spawn unbounded
  (oAuto, iAuto) <- spawn unbounded
  doAuto <- newIORef (view sAuto initialState)
  runEitherPrintError "controller error" (controllers o)
  ctlAuto oAuto
  print initialState
  runMVC initialState (asPipe $ loop model')
    ((,) <$> pure (asSink (render' doAuto)) <*> pure (cAuto iAuto) <> (fmap ActionIn <$> cAction i doAuto))

render' :: IORef Auto -> Out -> IO ()
render' _ (InOut i) = print i
render' _ (TodosOut tds) = do
  print tds
  render tds
render' ref (AutoOut auto) = do
  modifyIORef ref toggle
  renderAuto auto

renderAuto :: Auto -> IO ()
renderAuto auto = do
  print auto
  el <- element ".toggle-auto"
  case auto of
    Automatic -> setValue el ("on" :: String)
    Manual -> setValue el ("off" :: String)

cAuto :: Input Auto -> Controller In
cAuto iAuto = AutoIn <$> asInput iAuto

cAction :: Input (Action String) -> IORef Auto -> Managed (Controller (Action String))
cAction iAction doAuto =
  (pure (asInput iAction) <>
   producer unbounded (yield Refresh) <>
   producer unbounded
     (forever $ do
        lift (threadDelay (1000000 * 3))
        ref <- lift $ readIORef doAuto
        when (ref == Automatic) $ do
          x <- lift (generate arbitrary)
          yield x))

main :: IO ()
main = onload (void run)
