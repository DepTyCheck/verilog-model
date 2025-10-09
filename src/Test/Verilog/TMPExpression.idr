module Test.Verilog.TMPExpression

import Data.Fuel

import public Test.Verilog.Literal
import public Test.Verilog.SVType
import public Test.Verilog.Connections

import Test.DepTyCheck.Gen

-- TODO. Replace this with a real expression
public export
data TMPExpression : (mcs : MultiConnectionsList ms m subMs) -> (t : SVType) -> Type where
  MkQualName : (f : Fin $ length mcs) -> CanConnect (valueOf $ typeOf $ index mcs f) t => TMPExpression {ms} {m} {subMs} mcs t
  MkLiteral  : TypeLiteral t -> TMPExpression {ms} {m} {subMs} mcs t

public export
data TMPExList : (mcs : MultiConnectionsList ms m subMs) -> FinsList (length mcs) -> Type where
  Nil  : TMPExList mcs []
  (::) : TMPExpression mcs (valueOf $ typeOf $ index mcs f) -> TMPExList mcs fs -> TMPExList mcs (f::fs)

export
genTMPExList : Fuel -> {ms : _} -> {m : _} -> {subMs : _} ->
               (mcs : MultiConnectionsList ms m subMs) -> (fs : FinsList $ length mcs) -> Gen MaybeEmpty $ TMPExList mcs fs
