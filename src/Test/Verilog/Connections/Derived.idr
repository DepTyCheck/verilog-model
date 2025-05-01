module Test.Verilog.Connections.Derived

import Deriving.DepTyCheck.Gen

import public Test.Verilog.Connections

%default total

%logging "deptycheck" 20

Test.Verilog.Connections.genNotEqFin = deriveGen
Test.Verilog.Connections.genSourceForSink = deriveGen
Test.Verilog.Connections.genModules  = deriveGen
