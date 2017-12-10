module Eval where

import qualified Language.Haskell.Interpreter as I
import Data.Typeable

eval :: (MonadMask m, I.MonadIO m) => String -> m (Either I.InterpreterError String)
eval e = I.runInterpreter $ I.eval e -- >> I.eval e
-- TEST WITH mueval
