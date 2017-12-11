module Eval where

import           Control.Monad.Catch          (MonadMask)
import           Data.Typeable
import qualified Language.Haskell.Interpreter as I

-- TODO: dead with error handling
eval :: (MonadMask m, I.MonadIO m) => String -> m (Either I.InterpreterError String)
eval e = I.runInterpreter $ I.setImports ["Prelude"] >> I.eval e -- >> I.eval e
-- TEST WITH mueval
