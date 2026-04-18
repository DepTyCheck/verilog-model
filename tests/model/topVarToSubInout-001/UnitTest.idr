module UnitTest

import Test.Common.DataType
import Test.Common.Design
import Test.Common.Utils
import Test.Verilog.SVType

%default total

-- Reproduces the scenario:
--
--   module qpthxgpgv (inout tri1 logic xibrlho);
--   endmodule
--
--   module utidq (output logic ynhlzhtg);
--     qpthxgpgv ap(.xibrlho(ynhlzhtg));
--   endmodule
--
-- slang rejects this with:
--   error: 'ynhlzhtg' cannot be connected to 'inout' port (only nets are allowed)
--
-- IEEE 1800-2023 §23.3.3.2 — "A variable data type is not permitted on either
-- side of an inout port."

sig : DesignUnitSig SystemVerilog
sig = MkDesignUnitSig [MkPort (SVT $ Var $ AVar Logic') (SVP Out)]

subSig : DesignUnitSig SystemVerilog
subSig = MkDesignUnitSig [MkPort (SVT $ Net Tri1' (AVar Logic')) (SVP InOut)]

usl : DesignUnitSigsList SystemVerilog
usl = [subSig]

subUs : FinsList 1
subUs = [FZ]

-- Pre-state MC: top output variable alone, no sub wired in yet.
topMC : MultiConnection SystemVerilog UnitTest.sig UnitTest.usl UnitTest.subUs
topMC = MkMC Nothing (Just FZ) [] []

-- Post-bad-state MC: sub's inout net port already merged into the top-output
-- variable's MC — the wiring slang flags as illegal.
badMC : MultiConnection SystemVerilog UnitTest.sig UnitTest.usl UnitTest.subUs
badMC = MkMC Nothing (Just FZ) [FZ] []

-- Same bad MC, but with an explicit `ne = JustSSC` witness. Any generator
-- that can build this MC with this witness breaks the predicates downstream,
-- because `rawTypeOf` pattern-matches on `ne` and ends up returning the sub
-- port's type instead of the top port's type.
badMC_JustSSC : MultiConnection SystemVerilog UnitTest.sig UnitTest.usl UnitTest.subUs
badMC_JustSSC = MkMC Nothing (Just FZ) [FZ] [] @{JustSSC}

-- `canDrive` at the pre-state legitimately accepts this sub port: the MC
-- has no source yet (`noSource=True`), so adding a first source passes the
-- multidrive guard. It is NOT canDrive's job to reject the Var-to-inout
-- mismatch — that belongs to `portModesCompatible` / `forbidVarInout`.
canDrivePreStateAllows : canDrive UnitTest.topMC FZ = True
canDrivePreStateAllows = Refl

-- `portModesCompatible` must reject: variable data type on either side of an
-- inout port is forbidden.
portModesCompatiblePreStateRejects : portModesCompatible UnitTest.topMC FZ = False
portModesCompatiblePreStateRejects = Refl

-- Composite predicate must reject.
isGoodMCPreStateRejects : isGoodMC UnitTest.topMC FZ = False
isGoodMCPreStateRejects = Refl

-- Post-state: the MC already pairs the Var top port with the sub inout net.
-- Every predicate that inspects connection validity must still flag it.
canDriveBadStateRejects : canDrive UnitTest.badMC FZ = False
canDriveBadStateRejects = Refl

portModesCompatibleBadStateRejects : portModesCompatible UnitTest.badMC FZ = False
portModesCompatibleBadStateRejects = Refl

isGoodMCBadStateRejects : isGoodMC UnitTest.badMC FZ = False
isGoodMCBadStateRejects = Refl

-- The predicate should ALSO reject the MC when the `MCNotEmpty` witness
-- is `JustSSC`, because the structural wiring is identical — only the
-- proof term differs.
--
-- Currently this fails: `rawTypeOf` dispatches on `ne`, so under
-- `JustSSC` it returns the sub port's type (Net Tri1 logic) instead of
-- the top port's (Var logic). `forbidVarInout` then sees Net-vs-Net and
-- returns True, and `portModesCompatible` wrongly allows the connection.
-- This is the predicate bug that lets the generator emit the illegal
-- `.xibrlho(ynhlzhtg)` wiring.
portModesCompatibleBadStateRejectsUnderJustSSC
  : portModesCompatible UnitTest.badMC_JustSSC FZ = False
portModesCompatibleBadStateRejectsUnderJustSSC = Refl
