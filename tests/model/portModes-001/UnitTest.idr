module UnitTest

import Test.Common.DataType
import Test.Common.Design
import Test.Common.Utils
import Test.Verilog.SVType

%default total

sig : DesignUnitSig SystemVerilog
sig = MkDesignUnitSig [MkPort (SVT $ Var $ AVar Logic') (SVP Out)]

subSig : DesignUnitSig SystemVerilog
subSig = MkDesignUnitSig [MkPort (SVT $ Net Supply0' (AVar Logic')) (SVP InOut)]

usl : DesignUnitSigsList SystemVerilog
usl = [subSig]

subUs : FinsList 1
subUs = [FZ]

-- Bad connection with actual out var to formal inout net
mc : MultiConnection SystemVerilog UnitTest.sig UnitTest.usl UnitTest.subUs
mc = MkMC Nothing (Just FZ) [FZ] []

||| 23.3.3.2 — a variable is not permitted on either side of an `inout`
||| port. Connecting top `output logic qttx` (Var) to sub `inout supply0
||| logic shc` (InOut) violates the rule, so `portModesCompatible` must
||| reject it.
outVarToInoutNetRejected : portModesCompatible UnitTest.mc FZ = False
outVarToInoutNetRejected = Refl
