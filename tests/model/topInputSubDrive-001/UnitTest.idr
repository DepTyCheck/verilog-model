module UnitTest

import Test.Common.DataType
import Test.Common.Design
import Test.Common.Utils
import Test.Verilog.SVType

%default total

-- Reproduces the bad wiring that `tests/printer/create-dir/expected`
-- (and `model-coverage/expected`) currently emits on line 21:
--
--   module lstii (input logic qhbsbofux, input logic javnvkdjpl);
--     and wv(javnvkdjpl, r, z);
--     // assign r = 'bzx;
--     // assign z = 'b101;
--   endmodule: lstii
--
-- (Top port `qhbsbofux` in the actual output has packed dims; stripped
-- here per request — they don't affect the predicates at stake.)
--
-- `and wv(javnvkdjpl, r, z);` means `and`'s output (the first positional
-- arg) drives `javnvkdjpl`, which is declared `input logic`. Illegal per
-- IEEE 1800-2023 §23.3.3.2.

sig : DesignUnitSig SystemVerilog
sig = MkDesignUnitSig
  [ MkPort (SVT $ Var $ AVar Logic') (SVP In)   -- qhbsbofux
  , MkPort (SVT $ Var $ AVar Logic') (SVP In)   -- javnvkdjpl
  ]

-- Shape of an `and` primitive: out, in, in — all logic variables.
andSig : DesignUnitSig SystemVerilog
andSig = MkDesignUnitSig
  [ MkPort (SVT $ Var $ AVar Logic') (SVP Out)
  , MkPort (SVT $ Var $ AVar Logic') (SVP In)
  , MkPort (SVT $ Var $ AVar Logic') (SVP In)
  ]

usl : DesignUnitSigsList SystemVerilog
usl = [andSig]

subUs : FinsList 1
subUs = [FZ]

-- Fin (totalTops' sig) = Fin 2. FZ=qhbsbofux, FS FZ=javnvkdjpl.
javnvkdjpl : Fin 2
javnvkdjpl = FS FZ

-- Fin (totalSubs' usl subUs) = Fin 3.
andOut : Fin 3
andOut = FZ

andIn1 : Fin 3
andIn1 = FS FZ

andIn2 : Fin 3
andIn2 = FS (FS FZ)

-- Pre-state of the MC that names `javnvkdjpl`: just the top input alone.
topMCJavnvkdjpl : MultiConnection SystemVerilog UnitTest.sig UnitTest.usl UnitTest.subUs
topMCJavnvkdjpl = MkMC (Just UnitTest.javnvkdjpl) Nothing [] []

-- Post-bad-state MC as emitted by the generator: javnvkdjpl's MC now also
-- carries `and`'s output port.
badMCJavnvkdjpl : MultiConnection SystemVerilog UnitTest.sig UnitTest.usl UnitTest.subUs
badMCJavnvkdjpl = MkMC (Just UnitTest.javnvkdjpl) Nothing [UnitTest.andOut] []

-- Same bad MC, but with an explicit `ne = JustSSC` witness. Data identical,
-- proof term differs. `rawTypeOf` dispatches on `ne`, so this witness must
-- not change any downstream predicate.
badMCJavnvkdjplJustSSC : MultiConnection SystemVerilog UnitTest.sig UnitTest.usl UnitTest.subUs
badMCJavnvkdjplJustSSC = MkMC (Just UnitTest.javnvkdjpl) Nothing [UnitTest.andOut] [] @{JustSSC}


-- == typeOf ================================================================
--
-- `typeOf mc` must report the top port's type (Var logic) whenever the MC
-- contains a top port, no matter which `MCNotEmpty` witness was picked.

typeOfTopMCIsVar : typeOf UnitTest.topMCJavnvkdjpl = SVT (Var (AVar Logic'))
typeOfTopMCIsVar = Refl

typeOfBadMCIsVar : typeOf UnitTest.badMCJavnvkdjpl = SVT (Var (AVar Logic'))
typeOfBadMCIsVar = Refl

typeOfBadMCJustSSCIsVar : typeOf UnitTest.badMCJavnvkdjplJustSSC = SVT (Var (AVar Logic'))
typeOfBadMCJustSSCIsVar = Refl


-- == canDrive ==============================================================

canDriveTopMCRejectsAndOut : canDrive UnitTest.topMCJavnvkdjpl UnitTest.andOut = False
canDriveTopMCRejectsAndOut = Refl

canDriveBadMCRejectsAndOut : canDrive UnitTest.badMCJavnvkdjpl UnitTest.andOut = False
canDriveBadMCRejectsAndOut = Refl

canDriveBadMCJustSSCRejectsAndOut : canDrive UnitTest.badMCJavnvkdjplJustSSC UnitTest.andOut = False
canDriveBadMCJustSSCRejectsAndOut = Refl


-- == portModesCompatible ===================================================

portModesCompatibleTopMCRejectsAndOut : portModesCompatible UnitTest.topMCJavnvkdjpl UnitTest.andOut = False
portModesCompatibleTopMCRejectsAndOut = Refl

portModesCompatibleBadMCRejectsAndOut : portModesCompatible UnitTest.badMCJavnvkdjpl UnitTest.andOut = False
portModesCompatibleBadMCRejectsAndOut = Refl

portModesCompatibleBadMCJustSSCRejectsAndOut : portModesCompatible UnitTest.badMCJavnvkdjplJustSSC UnitTest.andOut = False
portModesCompatibleBadMCJustSSCRejectsAndOut = Refl


-- == isGoodMC ==============================================================

isGoodMCTopMCRejectsAndOut : isGoodMC UnitTest.topMCJavnvkdjpl UnitTest.andOut = False
isGoodMCTopMCRejectsAndOut = Refl

isGoodMCBadMCRejectsAndOut : isGoodMC UnitTest.badMCJavnvkdjpl UnitTest.andOut = False
isGoodMCBadMCRejectsAndOut = Refl

isGoodMCBadMCJustSSCRejectsAndOut : isGoodMC UnitTest.badMCJavnvkdjplJustSSC UnitTest.andOut = False
isGoodMCBadMCJustSSCRejectsAndOut = Refl


-- == goodFins ==============================================================
--
-- `SExisting` gates sub-port merging with `f : Fin (goodFins mid k).length`.
-- For the pre-state mid that buildTopMCS would produce (one MC per top
-- port), goodFins at k=0 (= and's output, sub source) must drop every
-- top-input MC. If non-empty, `SExisting` can thread the illegal wiring.

buildTopMidApprox : MultiConnectionsList SystemVerilog UnitTest.sig UnitTest.usl UnitTest.subUs
buildTopMidApprox =
  [ MkMC (Just FZ)                     Nothing [] []
  , MkMC (Just UnitTest.javnvkdjpl)    Nothing [] []
  ]

goodFinsDropsBothTopMCs : goodFins UnitTest.buildTopMidApprox 0 = []
goodFinsDropsBothTopMCs = Refl
