module Test.Verilog.Connections.Derived

import Deriving.DepTyCheck.Gen

import public Test.Verilog.Connections

%default total

%logging "deptycheck" 20

-- Test.Verilog.Connections.genMF = deriveGen
-- Test.Verilog.Connections.genCC = deriveGen
-- Test.Verilog.Connections.genFNI = deriveGen
Test.Verilog.Connections.genPf = genPf
Test.Verilog.Connections.genCSS = genCSS
Test.Verilog.Connections.genModules = deriveGen
