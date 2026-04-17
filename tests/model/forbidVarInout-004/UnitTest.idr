module UnitTest

import Test.Common.DataType
import Test.Common.Design
import Test.Verilog.SVType

%default total

||| Neither port is `inout`. `forbidVarInout` has nothing to rule on and
||| must allow the combination, even when both types are variables.
noInoutDoesNotRestrict
  : forbidVarInout In (SVT $ Var (AVar Logic')) Out (SVT $ Var (AVar Logic')) = True
noInoutDoesNotRestrict = Refl
