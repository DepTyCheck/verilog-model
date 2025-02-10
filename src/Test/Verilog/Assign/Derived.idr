module Test.Verilog.Assign.Derived

import Deriving.DepTyCheck.Gen

import public Test.Verilog.Assign

%default total

%logging "deptycheck" 5

Test.Verilog.Assign.genAssigns  = deriveGen
