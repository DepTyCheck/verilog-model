module UnitTest

import Test.Common.DataType
import Test.Common.Design
import Test.Common.Utils
import Test.Verilog.SVType

%default total

sig : DesignUnitSig SystemVerilog
sig = MkDesignUnitSig [MkPort (SVT $ Net Wire' (AVar Logic')) (SVP In)]

subSig : DesignUnitSig SystemVerilog
subSig = MkDesignUnitSig [MkPort (SVT $ Var $ AVar Logic') (SVP Out)]

usl : DesignUnitSigsList SystemVerilog
usl = [subSig]

subUs : FinsList 1
subUs = [FZ]

-- Resolved net top input (`input wire logic`) stays multi-drivable
mc : MultiConnection SystemVerilog UnitTest.sig UnitTest.usl UnitTest.subUs
mc = MkMC (Just FZ) Nothing [FZ] []

subOutDrivingTopInputNetAllowed : portModesCompatible UnitTest.mc FZ = True
subOutDrivingTopInputNetAllowed = Refl
