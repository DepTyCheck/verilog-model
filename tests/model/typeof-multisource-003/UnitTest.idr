module UnitTest

import Test.Common.DataType
import Test.Common.Multiconnection
import Test.Common.Utils
import Test.Verilog.SVType

%default total

||| A lone unpacked variable output is still a variable. The multi-source
||| `typeOf` promotion must not fire when there is only one driver.
--
-- IEEE 1800-2023 §23.3.3.2: one output port may drive a variable.

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

singleVarUnpackedKept : typeOf UnitTest.mc = SVT (Var (UnpackedArr (AVar Logic') 0 3))
singleVarUnpackedKept = Refl
