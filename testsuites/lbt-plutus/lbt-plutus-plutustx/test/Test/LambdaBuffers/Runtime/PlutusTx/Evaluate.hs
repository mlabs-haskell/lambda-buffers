module Test.LambdaBuffers.Runtime.PlutusTx.Evaluate (evalScript, evalScriptHuge, evalScript', EvalError, Script (..), fromCompiledCode, applyArg) where

import Data.Functor (void)
import Plutarch (Script (Script))
import Plutarch.Evaluate (EvalError, evalScript, evalScript', evalScriptHuge)
import PlutusCore qualified as PLC
import PlutusTx (CompiledCode, Lift, applyCode, getPlc, liftCode)
import UntypedPlutusCore (
  Program (Program),
 )
import UntypedPlutusCore qualified as UPLC

fromCompiledCode :: CompiledCode a -> Script
fromCompiledCode cc = let (Program _x _y t) = getPlc cc in Script (Program () _y (UPLC.termMapNames UPLC.unNameDeBruijn (void t)))

applyArg :: PlutusTx.Lift PLC.DefaultUni a => CompiledCode (a -> b) -> a -> Either String (CompiledCode b)
applyArg fun arg = let (Program _x version _t) = getPlc fun in PlutusTx.applyCode fun (PlutusTx.liftCode version arg)
