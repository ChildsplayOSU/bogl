module Utils(evalTest) where
--
-- Utils.hs
--
-- Various testing utilities
--

import Runtime.Eval

-- used to extract value from expression
evalTest :: Eval Val -> Either Exception Val
evalTest ev = runEval ev emptyEnv []
