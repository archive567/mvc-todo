import qualified TestTodoModel

import           Control.Applicative
import           Test.Tasty
import           Test.Tasty.Hspec

-- the tests
tests :: IO TestTree
tests = testGroup "mvc-todo" <$> sequence
  [ testSpec "Todo" =<< TestTodoModel.tests
  ]

main :: IO ()
main = defaultMain =<< tests
