module Control.Error.Extended 
  ( module Control.Error.Extended
  , module Control.Error.Util
  , module Control.Error.Safe
  , module Control.Monad.Trans.Either
  , lift
  ) where

import Control.Error.Safe
import Control.Error.Util
import Control.Monad.Morph
import Control.Monad.Trans.Either
import Data.Monoid

runEitherPrintError :: (Show a, Monoid a) => a -> EitherT a IO () -> IO ()
runEitherPrintError prefix f = do
  res <- runEitherT f
  case res of
    Left e   -> print $ prefix <> e
    Right () -> return ()
