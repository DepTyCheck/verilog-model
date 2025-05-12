module Test.Verilog.Expression

import Data.Fuel
import Data.Vect
import public Data.Fin

import Test.DepTyCheck.Gen
import Test.DepTyCheck.Gen.Coverage

import public Test.Verilog.Module

import public Test.Verilog.Literal

import Deriving.DepTyCheck.Gen

%default total

||| IEEE 1800-2023, 11.2
||| An expression is a construct that combines operands with operators to produce a result that is a function of
||| the values of the operands and the semantic meaning of the operator. Any legal operand, such as a net bit-
||| select, without any operator is considered an expression. Wherever a value is needed in a SystemVerilog
||| statement, an expression can be used.
||| An operand can be one of the following:
||| — Constant literal number, including real literals
||| — String literal
||| — Parameter, including local and specify parameters
||| — Parameter bit-select or part-select, including local and specify parameters
||| — Net (see 6.7)
||| — Net bit-select or part-select
||| — Variable (see 6.8)
||| — Variable bit-select or part-select
||| — Structure, either packed or unpacked
||| — Structure member
||| — Packed structure bit-select or part-select
||| — Union, packed, unpacked, or tagged
||| — Union member
||| — Packed union bit-select or part-select
||| — Array, either packed or unpacked
||| — Packed array bit-select, part-select, element, or slice
||| — Unpacked array element bit-select or part-select, element, or slice
||| — A call to a user-defined function, system function, or method that returns any of the above
public export
data SVExpression : (expressionType : SVType) -> (ports : PortsList) -> (usedPorts : ListOfPortsIndices ports) -> Type

public export
data SVExpression : (expressionType : SVType) -> (ports : PortsList) -> (usedPorts : ListOfPortsIndices ports) -> Type where
  SVVarOrNet : {t : SVType} -> {ports : PortsList} -> (i : IndexInPorts t ports) -> SVExpression t ports [i]
  SVLogicConstant : SValue4 -> SVExpression (Var Logic') ports []
  SVWireConstant : SValue4 -> SVExpression (Var Wire') ports []
  SVUwireConstant : SValue4 -> SVExpression (Var Uwire') ports []
  SVBitConstant : SValue2 -> SVExpression (Var Bit') ports []
  SVUnpackedArrayConstant : BinaryList t (S (max s e `minus` min s e)) -> SVExpression (Arr $ Unpacked t s e) ports []
  SVPackedArrayConstant : AllowedInPackedArr t => BinaryList t (S (max s e `minus` min s e)) -> SVExpression (Arr $ Packed t s e) ports []
  SVIndexUnpackedArray : (i : IndexInPorts (Arr $ Unpacked t s e) ports) -> (arrIndex : Nat) => LTE (min s e) arrIndex => LTE arrIndex (max s e) => SVExpression t ports [i]
  SVIndexPackedArray : AllowedInPackedArr t => (i : IndexInPorts (Arr $ Packed t s e) ports) -> (arrIndex : Nat) => LTE (min s e) arrIndex => LTE arrIndex (max s e) => SVExpression t ports [i]

export
genExpressions : Fuel -> (expressionType : SVType) -> (ports : PortsList) -> (usedPorts : ListOfPortsIndices ports) -> Gen MaybeEmpty (SVExpression expressionType ports usedPorts)
