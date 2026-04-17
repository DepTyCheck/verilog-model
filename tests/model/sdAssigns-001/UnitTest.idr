module UnitTest

import Test.Common.DataType
import Test.Common.Design
import Test.Common.Utils
import Test.Verilog.Assign
import Test.Verilog.SVType

%default total

sig : DesignUnitSig SystemVerilog
sig = MkDesignUnitSig [MkPort (SVT $ Var $ AVar Logic') (SVP In)]

usl : DesignUnitSigsList SystemVerilog
usl = []

subUs : FinsList 0
subUs = []

mc : MultiConnection SystemVerilog UnitTest.sig UnitTest.usl UnitTest.subUs
mc = MkMC (Just FZ) Nothing [] []

mcs : MultiConnectionsList SystemVerilog UnitTest.sig UnitTest.usl UnitTest.subUs
mcs = [mc]

||| 23.3.3.2 Port connection rules for variables
||| Assignments to variables declared as input ports shall be illegal.
||| IEEE 1800-2023
sdAssignForbiddenToTopInput
  : sdFins {s = UnitTest.sig} {usl = UnitTest.usl} {subUs = UnitTest.subUs} {mcs = UnitTest.mcs} [FZ] = []
sdAssignForbiddenToTopInput = Refl
