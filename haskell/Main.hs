{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Error.Extended (runEitherPrintError)
import qualified Data.Map as Map
import           GHCJS.Extended (onload)
import           MVC
import           MVC.Prelude as MVC
import           Todo.Controllers (controllers)
import           Todo.Model
import           Todo.Views (render)

main :: IO ()
main = onload (void run)

initialState :: Todos String
initialState =
  Todos "" Nothing (ItemId 3) Nothing
    (Map.fromList $ 
     zip (ItemId <$> [0 ..])
     [ Item Active "write view"
     , Item Active "write controllers"
     , Item Completed "render a todo list"
     ])

run :: IO (Todos String)
run = do
  (o, i) <- spawn unbounded
  runEitherPrintError "controller error" (controllers o)
  runMVC initialState (asPipe model')
    ((,) <$> 
     pure (asSink render') <*> 
     ( pure (asInput i) <> 
       producer unbounded (yield Refresh)))

render' :: Out -> IO ()
render' (ActionOut action) = print action
render' (StateOut tds) = render tds

-- FIXME: the view below doesn't work.  Issue with single-threadedness???
testMVC' :: IO (Todos String)
testMVC' = do
  (o, i) <- spawn unbounded
  runEitherPrintError "controller error" (controllers o)
  runMVC initialState (asPipe model')
    ((,) <$> (  (handles _StateOut  <$> pure (asSink render))
             <> (handles _ActionOut <$> pure (asSink print)))
         <*> (  pure (asInput i)
             <> producer unbounded (yield Refresh)))
