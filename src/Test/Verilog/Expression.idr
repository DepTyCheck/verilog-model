module Test.Verilog.Expression

import public Test.Common.Multiconnection

import public Test.Verilog.Literal


-- TODO. Replace this with a real expression
public export
data SVTMPExpression : (mcs : MultiConnectionsList SystemVerilog s usl subUs) -> (t : SVObject) -> Type where
  MkQualName : (f : Fin $ length mcs) -> So (equivalentSVO (dtToSVt $ typeOf $ index mcs f) t) => SVTMPExpression {s} {usl} {subUs} mcs t
  MkLiteral  : SVTypeLiteral (valueOf t) -> SVTMPExpression {s} {usl} {subUs} mcs t
