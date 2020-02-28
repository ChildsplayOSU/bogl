module Utils(evalTest) where
--
-- Utils.hs
--
-- Various testing utilities
--

import Runtime.Eval
import Runtime.Values
import Runtime.Monad

-- used to extract value from expression
evalTest :: Eval Val -> Either Exception Val
evalTest ev = runEval (emptyEnv (0,0)) [] ev
