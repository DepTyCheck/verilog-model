module Test.Common.Design.Derived

import Deriving.DepTyCheck.Gen

import public Test.Common.Design

%default total

%logging "deptycheck" 20

GenOrderTuning "SExisting".dataCon where
  isConstructor = itIsConstructor
  deriveFirst _ _ = [`{recur}, `{mid}, `{f}, `{jf}, `{cc}]

GenOrderTuning "GenMC".dataCon where
  isConstructor = itIsConstructor
  deriveFirst _ _ = [`{ft}, `{filledTop}, `{fs}, `{filledSub}]

Test.Common.Design.genDesignUnitsList = deriveGen
