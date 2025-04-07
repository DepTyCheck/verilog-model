module Test.Verilog.Module.Derived

import Deriving.DepTyCheck.Gen

import public Test.Verilog.Module

%default total

%logging "deptycheck" 20

Test.Verilog.Module.genNotEqFin = deriveGen
Test.Verilog.Module.genSourceForSink = deriveGen
Test.Verilog.Module.genModules  = deriveGen
