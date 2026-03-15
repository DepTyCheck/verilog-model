module Test.Common.Design.Derived

import Deriving.DepTyCheck.Gen

import public Test.Common.Design

%default total

%logging "deptycheck" 20

GenOrderTuning "SExisting".dataCon where
  isConstructor = itIsConstructor
  deriveFirst _ _ = [`{recur}, `{mid}, `{f}]

GenOrderTuning "GenMC".dataCon where
  isConstructor = itIsConstructor
  deriveFirst _ _ = [`{fs}, `{filledSub}] -- `{ft}, `{filledTop}, 

Test.Common.Design.genDesignUnitsList = deriveGen
