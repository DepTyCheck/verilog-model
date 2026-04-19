module UnitTest

import Test.Common.DataType
import Test.Common.Design
import Test.Verilog.SVType

%default total

||| 23.3.3.2 — a variable is not permitted on either side of an `inout`
||| port. When the formal port is `inout net` and the actual port carries a
||| variable, the combination must be rejected.
formalInoutNetVsActualOutVarRejected
  : forbidVarInout InOut (SVT $ Net Supply0' (AVar Logic')) Out (SVT $ Var (AVar Logic')) = False
formalInoutNetVsActualOutVarRejected = Refl
