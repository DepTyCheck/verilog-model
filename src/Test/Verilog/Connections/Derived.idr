module Test.Verilog.Connections.Derived

import Deriving.DepTyCheck.Gen

import public Test.Verilog.Connections

%default total

%logging "deptycheck" 20

GenOrderTuning "MkG".dataCon where
  isConstructor = itIsConstructor
  deriveFirst _ _ = [`{subMs}, `{m}, `{ftk}, `{fsk}, `{ftc}, `{fsc}]

GenOrderTuning "FACons".dataCon where
  isConstructor = itIsConstructor
  deriveFirst _ _ = [`{rest}, `{jf}, `{fit}]

GenOrderTuning "ExistingAny".dataCon where
  isConstructor = itIsConstructor
  deriveFirst _ _ = [`{f}, `{cap}, `{jmc}, `{cc}]

-- Test.Verilog.Connections.genCanAddPort = deriveGen
-- Test.Verilog.Connections.genFitAny  = deriveGen
-- Test.Verilog.Connections.genPrf     = deriveGen
Test.Verilog.Connections.genModules = deriveGen
