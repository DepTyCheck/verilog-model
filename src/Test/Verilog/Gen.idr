module Test.Verilog.Gen

import Deriving.DepTyCheck.Gen

import public Test.Verilog

%default total

%logging "deptycheck" 5

%hint LE : ConstructorDerivator; LE = LeastEffort {simplificationHack = True}

-- Test.Verilog.genModules = deriveGen

Test.Verilog.genModuleList = deriveGen