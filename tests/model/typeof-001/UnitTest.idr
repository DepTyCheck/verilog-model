module UnitTest

import Test.Common.DataType
import Test.Common.Design
import Test.Common.Utils
import Test.Verilog.SVType

%default total

sig : DesignUnitSig SystemVerilog
sig = MkDesignUnitSig [MkPort (SVT $ Var $ AVar Logic') (SVP Out)]

usl : DesignUnitSigsList SystemVerilog
usl = []

subUs : FinsList 0
subUs = []

mc : MultiConnection SystemVerilog UnitTest.sig UnitTest.usl UnitTest.subUs
mc = MkMC Nothing (Just FZ) [] []

||| When a `MultiConnection` is attached to a
||| top port, its type must be the declared top port type
topPortTypePreserved : typeOf UnitTest.mc = SVT (Var (AVar Logic'))
topPortTypePreserved = Refl
