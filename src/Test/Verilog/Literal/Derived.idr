module Test.Verilog.Literal.Derived

import Deriving.DepTyCheck.Gen

import public Test.Verilog.Literal

%default total

%logging "deptycheck" 20

Test.Verilog.Literal.genSingleBit  = deriveGen
