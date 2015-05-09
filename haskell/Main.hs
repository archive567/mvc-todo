{-# LANGUAGE OverloadedStrings #-}

module Main where

import Todo.Model
import Todo.Views
import Todo.Controllers
import GHCJS.Extended
import Control.Error.Extended
import MVC
import MVC.Prelude as MVC
import qualified Data.Map as Map

main :: IO ()
main = onload (void testMVC)

testState :: Todos String
testState = 
  Todos "" Nothing 3 Nothing 
  (Map.fromList $ zip [0..] 
   [ Item Active "write view"
   , Item Active "write controllers"
   , Item Completed "render a todo list"])

testMVC :: IO (Todos String)
testMVC = do
  (o, i) <- spawn unbounded
  -- render testState
  runEitherPrintError "controller error" (controllers o)
  runMVC testState (asPipe model')
    ((,) <$>
     (  pure $ asSink render'
     ) <*>
     (  pure (asInput i)
     <> producer unbounded (yield Refresh)
     )
    )

render' :: Out -> IO ()
render' (ActionOut action) = print action
render' (StateOut tds) = render tds

-- FIXME: the view below doesn't work.  Issue with single-threadedness???
testMVC' :: IO (Todos String)
testMVC' = do
  (o, i) <- spawn unbounded
  runEitherPrintError "controller error" (controllers o)
  runMVC testState (asPipe model')
    ((,) <$>
     (  ((handles _StateOut) <$> (pure $ asSink render))
     <> ((handles _ActionOut) <$> (pure $ asSink print))
     ) <*>
     (  pure (asInput i)
     <> producer unbounded (yield Refresh)
     )
    )


