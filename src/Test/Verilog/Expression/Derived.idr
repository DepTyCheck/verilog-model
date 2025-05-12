module Test.Verilog.Expression.Derived

import Deriving.DepTyCheck.Gen

import public Test.Verilog.Expression

%default total

%logging "deptycheck" 20

Test.Verilog.Expression.genExpressions = deriveGen
