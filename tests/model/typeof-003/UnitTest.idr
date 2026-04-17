module UnitTest

import Test.Common.DataType
import Test.Common.Design
import Test.Common.Utils
import Test.Verilog.SVType

%default total

sig : DesignUnitSig SystemVerilog
sig = MkDesignUnitSig []

subSig : DesignUnitSig SystemVerilog
subSig = MkDesignUnitSig [MkPort (SVT $ Var $ UnpackedArr (AVar Logic') 0 3) (SVP Out)]

usl : DesignUnitSigsList SystemVerilog
usl = [subSig]

subUs : FinsList 1
subUs = [FZ]

mc : MultiConnection SystemVerilog UnitTest.sig UnitTest.usl UnitTest.subUs
mc = MkMC Nothing Nothing [FZ] []

||| When a `MultiConnection` has no top port and its sub-port type is an
||| `UnpackedArr`, `typeOf` preserves the unpacked type
unpackedSubKept : typeOf UnitTest.mc = SVT (Var (UnpackedArr (AVar Logic') 0 3))
unpackedSubKept = Refl
