module Test.Common.Design.Derived

import Deriving.DepTyCheck.Gen

import public Test.Common.Design

%default total

%logging "deptycheck" 20

GenOrderTuning "SExisting".dataCon where
  isConstructor = itIsConstructor
  deriveFirst _ _ = [`{recur}, `{mid}, `{f}]

GenOrderTuning "MkDesign".dataCon where
  isConstructor = itIsConstructor
  deriveFirst _ _ = [`{fs}, `{mcs}]

Test.Common.Design.genDesignUnitsList = deriveGen
