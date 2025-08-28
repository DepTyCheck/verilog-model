module Test.Verilog.TMPExpression

import Data.Fuel

import public Test.Verilog.Literal
import public Test.Verilog.SVType
import public Test.Verilog.Connections

import Test.DepTyCheck.Gen

-- TODO. Replace this with a real expression
public export
data TMPExpression : (mcsLength : Nat) -> (t : SVObject) -> Type where
  MkQualName : Fin ml                  -> TMPExpression ml t
  MkLiteral  : TypeLiteral (valueOf t) -> TMPExpression ml t

public export
data TMPExList : Nat -> Type where
  Nil  : TMPExList n
  (::) : TMPExpression n t -> TMPExList n -> TMPExList n

export
genTMPExpr : Fuel -> (mcsLength : Nat) -> (t : SVObject) -> Gen MaybeEmpty $ TMPExpression mcsLength t

export
genTMPExList : Fuel -> {mcs : MultiConnectionsList ms m subMs} -> FinsList (length mcs) -> Gen MaybeEmpty $ TMPExList $ length mcs
genTMPExList _ []      = pure []
genTMPExList x (f::fs) = do
  tmpExpr <- genTMPExpr x (length mcs) (typeOf $ index mcs f)
  rest <- genTMPExList x fs
  pure $ tmpExpr :: rest
