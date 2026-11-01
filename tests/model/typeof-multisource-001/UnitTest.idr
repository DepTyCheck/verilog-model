module UnitTest

import Test.Common.DataType
import Test.Common.Multiconnection
import Test.Common.Utils
import Test.Verilog.SVType

%default total

-- Reproduces the scenario:
--
--   module h (output tri logic [3:3] raavrsn [3:3],
--             output logic [1:1][3:3] bfqazmne [4:4]);
--   endmodule: h
--
--   module l ();
--     logic [1:1][3:3] koikgh [4:4];
--     h tcvpjhqoxw(.raavrsn(koikgh), .bfqazmne(koikgh));
--   endmodule: l
--
-- slang rejects this with a multiply-driven variable. `addSub` prepends, so
-- the variable output sits at the head of `ssc` after joining the `tri`
-- output. `typeOf` must keep the resolved net, not flip to the variable.
--
-- IEEE 1800-2023 §6.5 / §23.3.3.2: a variable may be written by one
-- continuous assignment or one port.

sig : DesignUnitSig SystemVerilog
sig = MkDesignUnitSig []

subSig : DesignUnitSig SystemVerilog
subSig = MkDesignUnitSig
  [ MkPort (SVT $ Net Tri' (UnpackedArr (AVar Logic') 0 3)) (SVP Out)
  , MkPort (SVT $ Var (UnpackedArr (AVar Logic') 0 3)) (SVP Out)
  ]

usl : DesignUnitSigsList SystemVerilog
usl = [subSig]

subUs : FinsList 1
subUs = [FZ]

-- Variable output prepended onto the tri connection, as `addSub` does.
mc : MultiConnection SystemVerilog UnitTest.sig UnitTest.usl UnitTest.subUs
mc = MkMC Nothing Nothing [FS FZ, FZ] []

||| Two output ports on one internal unpacked connection: a `tri` net and a
||| variable. `typeOf` must be the resolved net, not the prepended variable.
keepsResolvedNet : typeOf UnitTest.mc = SVT (Net Tri' (UnpackedArr (AVar Logic') 0 3))
keepsResolvedNet = Refl
