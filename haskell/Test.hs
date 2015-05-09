import qualified TestMVCTodo

import           Control.Applicative
import           Test.Tasty
import           Test.Tasty.Hspec

-- the tests
tests :: IO TestTree
tests = testGroup "mvc-todo" <$> sequence
  [ testSpec "MVC.Todo" =<< TestMVCTodo.tests
  ]

main :: IO ()
main = defaultMain =<< tests
