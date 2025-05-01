module Test.Verilog.CtxPorts.Derived

import Deriving.DepTyCheck.Gen

import public Test.Verilog.CtxPorts

%default total

%logging "deptycheck" 20

Test.Verilog.CtxPorts.genSinkType = deriveGen
Test.Verilog.CtxPorts.genSourceType = deriveGen
