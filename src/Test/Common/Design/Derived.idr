module Test.Common.Design.Derived

import Deriving.DepTyCheck.Gen

import public Test.Common.Design

%default total

%logging "deptycheck" 20

GenOrderTuning "GenMC".dataCon where
  isConstructor = itIsConstructor
  deriveFirst _ _ = [`{ft}, `{filledTop}, `{fs}, `{filledSub}]

Test.Common.Design.genDesignUnits = deriveGen
-- Test.Common.Design.debugGenMCs = deriveGen
