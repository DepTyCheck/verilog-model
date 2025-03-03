module Test.Verilog.Assign.Derived

import Deriving.DepTyCheck.Gen

import public Test.Verilog.Assign

%default total

%logging "deptycheck" 7

Test.Verilog.Assign.SD.genFNE = deriveGen
Test.Verilog.Assign.SD.genSingleDriven = deriveGen
Test.Verilog.Assign.MD.genMultiDriven = deriveGen
