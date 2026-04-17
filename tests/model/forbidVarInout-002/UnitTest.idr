module UnitTest

import Test.Common.DataType
import Test.Common.Design
import Test.Verilog.SVType

%default total

||| Mirror of `forbidVarInout-001`: when the *actual* port is `inout net`
||| and the *formal* port carries a variable, the rule still rejects —
||| the restriction is symmetric.
actualInoutNetVsFormalOutVarRejected
  : forbidVarInout Out (SVT $ Var (AVar Logic')) InOut (SVT $ Net Supply0' (AVar Logic')) = False
actualInoutNetVsFormalOutVarRejected = Refl
