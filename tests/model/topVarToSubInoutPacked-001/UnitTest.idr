module UnitTest

import Test.Common.DataType
import Test.Common.Design
import Test.Common.Utils
import Test.Verilog.SVType

%default total

-- Reproduces the scenario:
--
--   module lyaheus (inout trior logic [3:1][0:0] dnxamdhvo);
--   endmodule: lyaheus
--
--   module ksba (output logic jxmsubyqkx, input logic g);
--     lyaheus ruyws(.dnxamdhvo(jxmsubyqkx));
--   endmodule: ksba
--
-- slang rejects this with:
--   error: 'jxmsubyqkx' cannot be connected to 'inout' port (only nets are allowed)
--
-- IEEE 1800-2023 §23.3.3.2: "A variable data type is not permitted on either
-- side of an inout port."

-- Packed array type [3:1][0:0] logic.
lyaheusPacked : SVType
lyaheusPacked = PackedArr (PackedArr (AVar Logic') 0 0) 3 1

sig : DesignUnitSig SystemVerilog
sig = MkDesignUnitSig
  [ MkPort (SVT $ Var $ AVar Logic') (SVP Out)   -- jxmsubyqkx
  , MkPort (SVT $ Var $ AVar Logic') (SVP In)    -- g
  ]

subSig : DesignUnitSig SystemVerilog
subSig = MkDesignUnitSig
  [ MkPort (SVT $ Net Trior' UnitTest.lyaheusPacked) (SVP InOut) ]

usl : DesignUnitSigsList SystemVerilog
usl = [subSig]

subUs : FinsList 1
subUs = [FZ]

-- Top port indices: FZ = jxmsubyqkx, FS FZ = g.
jxmsubyqkx : Fin 2
jxmsubyqkx = FZ

-- Pre-state of jxmsubyqkx's MC: top output alone.
topMC : MultiConnection SystemVerilog UnitTest.sig UnitTest.usl UnitTest.subUs
topMC = MkMC Nothing (Just UnitTest.jxmsubyqkx) [] []

-- Post-bad-state MC: sub's packed inout net port merged into the top-output
-- Var MC — the wiring slang rejects.
badMC : MultiConnection SystemVerilog UnitTest.sig UnitTest.usl UnitTest.subUs
badMC = MkMC Nothing (Just UnitTest.jxmsubyqkx) [FZ] []

-- Same MC, but with an explicit `ne = JustSSC` witness. Data identical.
badMC_JustSSC : MultiConnection SystemVerilog UnitTest.sig UnitTest.usl UnitTest.subUs
badMC_JustSSC = MkMC Nothing (Just UnitTest.jxmsubyqkx) [FZ] [] @{JustSSC}


-- == typeOf ================================================================
--
-- `typeOf` must report the top port's type (Var logic) regardless of which
-- `MCNotEmpty` witness was picked.

typeOfTopMCIsVar : typeOf UnitTest.topMC = SVT (Var (AVar Logic'))
typeOfTopMCIsVar = Refl

typeOfBadMCIsVar : typeOf UnitTest.badMC = SVT (Var (AVar Logic'))
typeOfBadMCIsVar = Refl

typeOfBadMCJustSSCIsVar : typeOf UnitTest.badMC_JustSSC = SVT (Var (AVar Logic'))
typeOfBadMCJustSSCIsVar = Refl


-- == portModesCompatible ===================================================
--
-- `Var` on a top port connected to an `inout` net sub port must be rejected.

portModesCompatibleTopMCRejects : portModesCompatible UnitTest.topMC FZ = False
portModesCompatibleTopMCRejects = Refl

portModesCompatibleBadMCRejects : portModesCompatible UnitTest.badMC FZ = False
portModesCompatibleBadMCRejects = Refl

portModesCompatibleBadMCJustSSCRejects : portModesCompatible UnitTest.badMC_JustSSC FZ = False
portModesCompatibleBadMCJustSSCRejects = Refl
