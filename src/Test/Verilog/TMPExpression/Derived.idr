module Test.Verilog.TMPExpression.Derived

import Deriving.DepTyCheck.Gen

import public Test.Verilog.TMPExpression

%default total

%logging "deptycheck" 20

Test.Verilog.TMPExpression.genTMPExpr = deriveGen
