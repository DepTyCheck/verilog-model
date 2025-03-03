module Test.Verilog.Module.Derived

import Deriving.DepTyCheck.Gen

import public Test.Verilog.Module

%default total

%logging "deptycheck" 7

Test.Verilog.Module.genModules  = deriveGen
