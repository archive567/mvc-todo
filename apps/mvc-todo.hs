{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad.Trans.State.Strict (State, StateT)
import MVC hiding ((<>))
import MVC.Prelude as MVC
import Protolude hiding (State, StateT, loop)
import Todo.Controllers (controllers)
import Todo.Model
import Todo.Vanilla (onload)
import Todo.Views (render)
import qualified Data.Map as Map

main :: IO ()
main = onload (void run)

initialState :: Todos
initialState =
  Todos "" Nothing (ItemId 3) Nothing
    (Map.fromList $ 
     zip (ItemId <$> [0 ..])
     [ Item Active "write view"
     , Item Active "write controllers"
     , Item Completed "render a todo list"
     ])

run :: IO Todos
run = do
  (o, i) <- spawn unbounded
  controllers o
  runMVC initialState (asPipe $ MVC.loop model)
    ((,) <$> 
     pure (asSink render_) <*> 
     ( pure (asInput i) `mappend`
       producer unbounded (yield Refresh)))

render_ :: Out -> IO ()
render_ (ActionOut action) = print action
render_ (StateOut tds) = render tds
