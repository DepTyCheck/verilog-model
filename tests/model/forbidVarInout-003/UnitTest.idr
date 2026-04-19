module UnitTest

import Test.Common.DataType
import Test.Common.Design
import Test.Verilog.SVType

%default total

||| Both sides are nets and at least one mode is `inout` — this is the
||| canonical valid `inout` connection and must be allowed.
bothInoutBothNetAllowed
  : forbidVarInout InOut (SVT $ Net Supply0' (AVar Logic')) InOut (SVT $ Net Wire' (AVar Logic')) = True
bothInoutBothNetAllowed = Refl
