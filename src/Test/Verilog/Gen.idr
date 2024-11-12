module Test.Verilog.Gen

import Deriving.DepTyCheck.Gen

import public Test.Verilog

%default total

%logging "deptycheck" 5

Test.Verilog.genModules = deriveGen
