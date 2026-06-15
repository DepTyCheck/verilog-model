module Test.VHDL.Expression

import public Test.Common.Multiconnection

import public Test.VHDL.Literal

public export
data VHDLExpression : (mcs : MultiConnectionsList VHDL s usl subUs) -> (t : VHDLType) -> Type where
  -- MkQualName : (f : Fin $ length mcs) ->  SVTMPExpression {s} {usl} {subUs} mcs t -- So (equivalentSVO (dtToSVt $ typeOf $ index mcs f) t) =>
  MkLiteral  : VHDLLiteral t -> VHDLExpression {s} {usl} {subUs} mcs t
