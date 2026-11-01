module UnitTest

import Test.Common.DataType
import Test.Common.Multiconnection
import Test.Common.Utils
import Test.Verilog.SVType

%default total

-- Three outputs on one internal unpacked connection: one `tri` net and two
-- variables, with both variables prepended (as repeated `addSub` does).
--
-- IEEE 1800-2023 §6.5 / §23.3.3.2: the declared signal must stay a
-- resolved net so multiple port drivers remain legal.

sig : DesignUnitSig SystemVerilog
sig = MkDesignUnitSig []

subSig : DesignUnitSig SystemVerilog
subSig = MkDesignUnitSig
  [ MkPort (SVT $ Net Tri' (UnpackedArr (AVar Logic') 0 3)) (SVP Out)
  , MkPort (SVT $ Var (UnpackedArr (AVar Logic') 0 3)) (SVP Out)
  , MkPort (SVT $ Var (UnpackedArr (AVar Logic') 0 3)) (SVP Out)
  ]

usl : DesignUnitSigsList SystemVerilog
usl = [subSig]

subUs : FinsList 1
subUs = [FZ]

mc : MultiConnection SystemVerilog UnitTest.sig UnitTest.usl UnitTest.subUs
mc = MkMC Nothing Nothing [FS (FS FZ), FS FZ, FZ] []

||| Three mixed outputs (net + variables). `typeOf` must still pick the
||| resolved net, not whichever variable is currently at the head of `ssc`.
keepsResolvedNetAmongThree : typeOf UnitTest.mc = SVT (Net Tri' (UnpackedArr (AVar Logic') 0 3))
keepsResolvedNetAmongThree = Refl
