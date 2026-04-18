module UnitTest

import Test.Common.DataType
import Test.Common.Design
import Test.Common.Utils
import Test.Verilog.SVType

%default total

sig : DesignUnitSig SystemVerilog
sig = MkDesignUnitSig [MkPort (SVT $ Var $ AVar Logic') (SVP In)]

subSig : DesignUnitSig SystemVerilog
subSig = MkDesignUnitSig [MkPort (SVT $ Var $ AVar Logic') (SVP Out)]

usl : DesignUnitSigsList SystemVerilog
usl = [subSig]

subUs : FinsList 1
subUs = [FZ]

-- Top input Var (`input logic`) must not be driven by a submodule output port
mc : MultiConnection SystemVerilog UnitTest.sig UnitTest.usl UnitTest.subUs
mc = MkMC (Just FZ) Nothing [FZ] []

||| 23.3.3.2 Port connection rules for variables
||| Assignments to variables declared as input ports shall be illegal.
||| IEEE 1800-2023
subOutDrivingTopInputVarRejected : portModesCompatible UnitTest.mc FZ = False
subOutDrivingTopInputVarRejected = Refl
