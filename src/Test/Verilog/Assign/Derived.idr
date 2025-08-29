module Test.Verilog.Assign.Derived

import Deriving.DepTyCheck.Gen

import public Test.Verilog.Assign

%default total

%logging "deptycheck" 20

GenOrderTuning "SDCons".dataCon where
  isConstructor = itIsConstructor
  deriveFirst _ _ = [`{rest}, `{f}, `{ns}, `{sd}, `{newUF}]

Test.Verilog.Assign.SD.genSDAssigns = deriveGen
Test.Verilog.Assign.MD.genMDAssigns = deriveGen
