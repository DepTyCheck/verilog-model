module Test.Verilog.Expression

import Data.Fuel
import Data.Vect

import public Data.So

import Test.DepTyCheck.Gen
import Test.DepTyCheck.Gen.Coverage

import public Test.Verilog.Module

import public Test.Verilog.Literal


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

||| The full list of available operators is described in the section 11.3 of the IEEE 1800-2023 standard.
||| We deviate from the standard in the following ways:
||| - Assignment operators are not included, they are considered the part of the statements instead.
||| - Ternary conditioanl operator is included dirtectly into the expression type.

||| Encodes that third SVType is the common supertype of the first two.
||| TODO: Introduce a propper order lattice structure to the type.
public export
data SupremumType : SVBasic -> SVBasic -> SVBasic -> Type where
  BitLogicLogic : SupremumType Bit' Logic' Logic'
  LogicWireWire : SupremumType Logic' Wire' Wire'
  BitWireWire : SupremumType Bit' Wire' Wire'
  SupremumSym : SupremumType x y z => SupremumType y x z
  SupremumReflex : SupremumType x x x

||| The following three types are used to represent compatible types of arrays.

public export
data ArrEnd : (start : Nat) -> (end : Nat) -> (finalIndex : Nat) -> Type where
  ArrEndA : So (start < end) -> So (finalIndex == (end `minus` start)) -> ArrEnd start end finalIndex
  ArrEndB : So (end < start) -> So (finalIndex == (start `minus` end)) -> ArrEnd start end finalIndex

public export
data ArrSizeSupremum : (start1 : Nat) -> (end1 : Nat) ->
                       (start2 : Nat) -> (end2 : Nat) ->
                       (startR : Nat) -> (endR : Nat) -> Type where
  ArrSizeSupremumConstructA : ArrEnd start1 end1 finalIndex1 ->
                              ArrEnd start2 end2 finalIndex2 ->
                              So (endR == (startR + (max finalIndex1 finalIndex2))) ->
                              ArrSizeSupremum start1 end1 start2 end2 startR endR
  ArrSizeSupremumConstructB : ArrEnd start1 end1 finalIndex1 ->
                              ArrEnd start2 end2 finalIndex2 ->
                              So (endR == (startR + (max finalIndex1 finalIndex2))) ->
                              ArrSizeSupremum start1 end1 start2 end2 endR startR

public export
data ArrSupremum : (SVArray t1 start1 end1) -> (SVArray t2 start2 end2) -> (SVArray tR startR endR) -> Type where
  ArrSupremumConstructA : SupremumType x y z ->
                          ArrSizeSupremum start1 end1 start2 end2 startR endR ->
                          ArrSupremum (Unpacked (Var x) start1 end1) (Unpacked (Var y) start2 end2) (Unpacked (Var z) startR endR)
  ArrSupremumConstructB : SupremumType x y z ->
                          ArrSizeSupremum start1 end1 start2 end2 startR endR ->
                          AllowedInPackedArr (Var x) => AllowedInPackedArr (Var y) => AllowedInPackedArr (Var z) =>
                          ArrSupremum (Packed (Var x) start1 end1) (Packed (Var y) start2 end2) (Packed (Var z) startR endR)
  ArrSupremumConstructC : ArrSupremum x y z ->
                          ArrSizeSupremum start1 end1 start2 end2 startR endR ->
                          ArrSupremum (Unpacked (Arr x) start1 end1) (Unpacked (Arr y) start2 end2) (Unpacked (Arr z) startR endR)
  ArrSupremumConstructD : ArrSupremum x y z ->
                          ArrSizeSupremum start1 end1 start2 end2 startR endR ->
                          (al1 : AllowedInPackedArr (Arr x)) => (al2 : AllowedInPackedArr (Arr y)) => (al3 : AllowedInPackedArr (Arr z)) =>
                          ArrSupremum (Packed @{al1} (Arr x) start1 end1) (Packed @{al2} (Arr y) start2 end2) (Packed @{al3} (Arr z) startR endR)

public export
data SVBinaryOperator : (lType : SVType) -> (rType : SVType) -> (retType : SVType) -> Type where
  SVLogicAnd : SupremumType x y z => SVBinaryOperator (Var x) (Var y) (Var z)
  SVLogicOr : SupremumType x y z => SVBinaryOperator (Var x) (Var y) (Var z)
  SVLogicImply : SupremumType x y z => SVBinaryOperator (Var x) (Var y) (Var z)
  SVLogicalEquiv : SupremumType x y z => SVBinaryOperator (Var x) (Var y) (Var z)
  SVBiteWiseAnd : ArrSupremum x y z => SVBinaryOperator (Arr x) (Arr y) (Arr z)
  SVBiteWiseOr : ArrSupremum x y z => SVBinaryOperator (Arr x) (Arr y) (Arr z)
  SVAddBits : SupremumType x y z => SVBinaryOperator (Var x) (Var y) (Var z)
  SVAddArrays : ArrSupremum x y z => SVBinaryOperator (Arr x) (Arr y) (Arr z)
  SVSubBits : SupremumType x y z => SVBinaryOperator (Var x) (Var y) (Var z)
  SVSubArrays : ArrSupremum x y z => SVBinaryOperator (Arr x) (Arr y) (Arr z)
  SVMultBits : SupremumType x y z => SVBinaryOperator (Var x) (Var y) (Var z)
  SVMultArrays : ArrSupremum x y z => SVBinaryOperator (Arr x) (Arr y) (Arr z)
  SVDivBits : SupremumType x y z => SVBinaryOperator (Var x) (Var y) (Var z)
  SVDivArrays : ArrSupremum x y z => SVBinaryOperator (Arr x) (Arr y) (Arr z)
  SVLessBits : SupremumType x y z => SVBinaryOperator (Var x) (Var y) (Var z)
  SVLessArrays : ArrSupremum x y z => SVBinaryOperator (Arr x) (Arr y) (Var Logic')
  SVLessEqBits : SupremumType x y z => SVBinaryOperator (Var x) (Var y) (Var z)
  SVLessEqArrays : ArrSupremum x y z => SVBinaryOperator (Arr x) (Arr y) (Var Logic')
  SVGreaterBits : SupremumType x y z => SVBinaryOperator (Var x) (Var y) (Var z)
  SVGreaterArrays : ArrSupremum x y z => SVBinaryOperator (Arr x) (Arr y) (Var Logic')
  SVLogicalEqualBits : SupremumType x y z => SVBinaryOperator (Var x) (Var y) (Var Logic')
  SVLogicalEqualArrays : ArrSupremum x y z => SVBinaryOperator (Arr x) (Arr y) (Var Logic')
  SVLogicalNotEqualBits : SupremumType x y z => SVBinaryOperator (Var x) (Var y) (Var Logic')
  SVLogicalNotEqualArrays : ArrSupremum x y z => SVBinaryOperator (Arr x) (Arr y) (Var Logic')
  SVCaseEqualBits : SupremumType x y z => SVBinaryOperator (Var x) (Var y) (Var Bit')
  SVCaseEqualArrays : ArrSupremum x y z => SVBinaryOperator (Arr x) (Arr y) (Var Bit')
  SVCaseNotEqualBits : SupremumType x y z => SVBinaryOperator (Var x) (Var y) (Var Bit')
  SVCaseNotEqualArrays : ArrSupremum x y z => SVBinaryOperator (Arr x) (Arr y) (Var Bit')

||| Predicate to test if the type is integral as described in the IEEE 1800-2023 standard.
public export
data IsIntegralType : SVType -> Type where
  LogicIsIntegral : IsIntegralType (Var Logic')
  BitIsIntegral : IsIntegralType (Var Bit')
  WireIsIntegral : IsIntegralType (Var Wire')
  UwireIsIntegral : IsIntegralType (Var Uwire')
  IntIsIntegral : IsIntegralType (Var Int')
  IntegerIsIntegral : IsIntegralType (Var Integer')
  UnpackedArrysIsIntegral : IsIntegralType t -> IsIntegralType (Arr (Unpacked t s e))
  PackedArrysIsIntegral : AllowedInPackedArr t => IsIntegralType t -> IsIntegralType (Arr (Packed t s e))

public export
data IsLogicOrBite : SVType -> Type where
  LogicIs : IsLogicOrBite (Var Logic')
  BitIs : IsLogicOrBite (Var Bit')
  WireIs : IsLogicOrBite (Var Wire')
  UwireIs : IsLogicOrBite (Var Uwire')

public export
data SVUnaryOperator : (iType : SVType) -> (oType : SVType) -> Type where
  SVUnaryMinus : IsIntegralType iType => SVUnaryOperator iType iType
  SVLogicalNegation : IsLogicOrBite iType => SVUnaryOperator iType iType
  SVBitwiseNegationUnpacked : IsIntegralType iType => SVUnaryOperator (Arr $ Unpacked iType s e) (Arr $ Unpacked iType s e)
  SVBitwiseNegationPacked : AllowedInPackedArr iType => IsIntegralType iType => SVUnaryOperator (Arr $ Packed iType s e) (Arr $ Packed iType s e)

public export
data SVExpression : (expressionType : SVType) -> (ports : PortsList) -> (usedPorts : ListOfPortsIndices ports) -> Type where
  SVVarOrNet : (i : IndexInPorts ports)-> AtIndexInPorts i t => SVExpression t ports [i]
  SVLogicConstant : SValue4 -> SVExpression (Var Logic') ports []
  SVWireConstant : SValue4 -> SVExpression (Var Wire') ports []
  SVUwireConstant : SValue4 -> SVExpression (Var Uwire') ports []
  SVBitConstant : SValue2 -> SVExpression (Var Bit') ports []
  SVUnpackedArrayConstant : BinaryList t (S (max s e `minus` min s e)) -> SVExpression (Arr $ Unpacked t s e) ports []
  SVPackedArrayConstant : AllowedInPackedArr t => BinaryList t (S (max s e `minus` min s e)) -> SVExpression (Arr $ Packed t s e) ports []
  SVIndexUnpackedArray : (i : IndexInPorts ports) -> AtIndexInPorts i (Arr $ Unpacked t s e) => (arrIndex : Nat) => LTE (min s e) arrIndex => LTE arrIndex (max s e) => SVExpression t ports [i]
  SVIndexPackedArray : AllowedInPackedArr t => (i : IndexInPorts ports) -> AtIndexInPorts i (Arr $ Packed t s e) => (arrIndex : Nat) -> LTE (min s e) arrIndex => LTE arrIndex (max s e) => SVExpression t ports [i]
  SVApplyBinaryOperator : {usedPortsL : _} -> {usedPortsR : _} ->
                          SVBinaryOperator lType rType retType ->
                          (l : SVExpression lType ports usedPortsL) -> (r : SVExpression rType ports usedPortsR) ->
                          {default (usedPortsL `union` usedPortsR) resUsedPorts : _} -> So (resUsedPorts `setEqual` (usedPortsL `union` usedPortsR)) =>
                          SVExpression retType ports resUsedPorts
  SVApplyUnaryOperator : {usedPorts : _} -> SVUnaryOperator iType oType -> SVExpression iType ports usedPorts -> SVExpression oType ports usedPorts
  -- TODO: Rework this part after the complete partial order of types is introduced.
  SVConditionalExpression : {usedPortsCond : _} -> {cType : SVType} -> IsLogicOrBite cType =>
                            SVExpression cType ports usedPortsCond ->
                            {tType : SVBasic} -> {fType : SVBasic} -> {rType : SVBasic} -> SupremumType tType fType rType =>
                            {usedPortsT : _} -> SVExpression (Var tType) ports usedPortsT ->
                            {usedPortsF : _} -> SVExpression (Var fType) ports usedPortsF ->
                            {default ((usedPortsCond `union` usedPortsT) `union` usedPortsF) resUsedPorts : _} -> So (resUsedPorts `setEqual` (((usedPortsCond `union` usedPortsT) `union` usedPortsF))) ->
                            SVExpression (Var rType) ports resUsedPorts
  SVCondtionExpressionArray : {usedPortsCond : _} -> {cType : SVType} -> IsLogicOrBite cType =>
                              SVExpression cType ports usedPortsCond ->
                              {t1 : _} -> {s1 : _} -> {e1 : _} -> {tType : SVArray t1 s1 e1} ->
                              {t2 : _} -> {s2 : _} -> {e2 : _} -> {fType : SVArray t2 s2 e2} ->
                              {t3 : _} -> {s3 : _} -> {e3 : _} -> {rType : SVArray t3 s3 e3} ->
                              ArrSupremum tType fType rType =>
                              {usedPortsT : _} -> SVExpression (Arr tType) ports usedPortsT ->
                              {usedPortsF : _} -> SVExpression (Arr fType) ports usedPortsF ->
                              {default ((usedPortsCond `union` usedPortsT) `union` usedPortsF) resUsedPorts : _} -> So (resUsedPorts `setEqual` (((usedPortsCond `union` usedPortsT) `union` usedPortsF))) ->
                              SVExpression (Arr rType) ports resUsedPorts


export
genExpressions : Fuel -> (expressionType : SVType) -> (ports : PortsList) -> (usedPorts : ListOfPortsIndices ports) -> Gen MaybeEmpty (SVExpression expressionType ports usedPorts)
