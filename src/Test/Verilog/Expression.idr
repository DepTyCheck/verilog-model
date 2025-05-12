module Test.Verilog.Expression

import Data.Fuel
import Data.Vect

import public Data.So

import Test.DepTyCheck.Gen
import Test.DepTyCheck.Gen.Coverage

import public Test.Verilog.SVType

import public Test.Verilog.Literal


%default total

public export
data SVReal : SVType -> Type where
  RealIsReal : SVReal $ RVar Real'
  ShortrealIsReal : SVReal $ RVar Shortreal'

public export
data SVIntegralOrReal : SVType -> Type where
  LeftSVIntegral : SVIntegral x -> SVIntegralOrReal x
  RightSVReal : SVReal x -> SVIntegralOrReal x

public export
data ExtendedReal : SVType -> Type where
  RealsAreExtendedReal : SVReal x -> ExtendedReal x
  RealtimeIsReal : ExtendedReal $ RVar Realtime'
  UnpackedOfRealsIsReal : ExtendedReal t -> ExtendedReal $ UnpackedArr t s e

public export
data NotExtendedReal : SVType -> Type where
  IntegralsAreNotReals : SVIntegral x -> NotExtendedReal x
  UnpackedOfNotRealsIsNotReal : NotExtendedReal t -> NotExtendedReal $ UnpackedArr t s e

0 ExtndedRealAndNotExtendedRealImpliesVoid : (t : SVType) -> NotExtendedReal t -> ExtendedReal t -> Void
ExtndedRealAndNotExtendedRealImpliesVoid (RVar _) (IntegralsAreNotReals ST) (RealsAreExtendedReal _) impossible
ExtndedRealAndNotExtendedRealImpliesVoid (RVar _) (IntegralsAreNotReals VT) (RealsAreExtendedReal _) impossible
ExtndedRealAndNotExtendedRealImpliesVoid (RVar _) (IntegralsAreNotReals PT) (RealsAreExtendedReal _) impossible
ExtndedRealAndNotExtendedRealImpliesVoid (SVar _) (IntegralsAreNotReals _) (RealsAreExtendedReal RealIsReal) impossible
ExtndedRealAndNotExtendedRealImpliesVoid (SVar _) (IntegralsAreNotReals _) (RealsAreExtendedReal ShortrealIsReal) impossible
ExtndedRealAndNotExtendedRealImpliesVoid (VVar _) (IntegralsAreNotReals _) (RealsAreExtendedReal RealIsReal) impossible
ExtndedRealAndNotExtendedRealImpliesVoid (VVar _) (IntegralsAreNotReals _) (RealsAreExtendedReal ShortrealIsReal) impossible
ExtndedRealAndNotExtendedRealImpliesVoid (PackedArr _ _ _) (IntegralsAreNotReals _) (RealsAreExtendedReal RealIsReal) impossible
ExtndedRealAndNotExtendedRealImpliesVoid (PackedArr _ _ _) (IntegralsAreNotReals _) (RealsAreExtendedReal ShortrealIsReal) impossible
ExtndedRealAndNotExtendedRealImpliesVoid (UnpackedArr _ _ _) (IntegralsAreNotReals ST) (RealsAreExtendedReal _) impossible
ExtndedRealAndNotExtendedRealImpliesVoid (UnpackedArr _ _ _) (IntegralsAreNotReals VT) (RealsAreExtendedReal _) impossible
ExtndedRealAndNotExtendedRealImpliesVoid (UnpackedArr _ _ _) (IntegralsAreNotReals PT) (RealsAreExtendedReal _) impossible
ExtndedRealAndNotExtendedRealImpliesVoid (RVar Realtime') (IntegralsAreNotReals ST) RealtimeIsReal impossible
ExtndedRealAndNotExtendedRealImpliesVoid (RVar Realtime') (IntegralsAreNotReals VT) RealtimeIsReal impossible
ExtndedRealAndNotExtendedRealImpliesVoid (RVar Realtime') (IntegralsAreNotReals PT) RealtimeIsReal impossible
ExtndedRealAndNotExtendedRealImpliesVoid (UnpackedArr _ _ _) (IntegralsAreNotReals ST) (UnpackedOfRealsIsReal _) impossible
ExtndedRealAndNotExtendedRealImpliesVoid (UnpackedArr _ _ _) (IntegralsAreNotReals VT) (UnpackedOfRealsIsReal _) impossible
ExtndedRealAndNotExtendedRealImpliesVoid (UnpackedArr _ _ _) (IntegralsAreNotReals PT) (UnpackedOfRealsIsReal _) impossible
ExtndedRealAndNotExtendedRealImpliesVoid (UnpackedArr _ _ _) (UnpackedOfNotRealsIsNotReal _) (RealsAreExtendedReal RealIsReal) impossible
ExtndedRealAndNotExtendedRealImpliesVoid (UnpackedArr _ _ _) (UnpackedOfNotRealsIsNotReal _) (RealsAreExtendedReal ShortrealIsReal) impossible
ExtndedRealAndNotExtendedRealImpliesVoid (UnpackedArr t s e) (UnpackedOfNotRealsIsNotReal x) (UnpackedOfRealsIsReal y) =
  ExtndedRealAndNotExtendedRealImpliesVoid t x y

0 TypeEitherRealOrNot : (t : SVType) -> Either (ExtendedReal t) (NotExtendedReal t)
TypeEitherRealOrNot (RVar Shortreal') = Left $ RealsAreExtendedReal ShortrealIsReal
TypeEitherRealOrNot (RVar Real') = Left $ RealsAreExtendedReal RealIsReal
TypeEitherRealOrNot (RVar Realtime') = Left RealtimeIsReal
TypeEitherRealOrNot (SVar _) = Right $ IntegralsAreNotReals ST
TypeEitherRealOrNot (VVar _) = Right $ IntegralsAreNotReals VT
TypeEitherRealOrNot (PackedArr _ _ _) = Right $ IntegralsAreNotReals PT
TypeEitherRealOrNot (UnpackedArr t _ _) with (TypeEitherRealOrNot t)
  TypeEitherRealOrNot (UnpackedArr t _ _) | (Left x) = Left $ UnpackedOfRealsIsReal x
  TypeEitherRealOrNot (UnpackedArr t _ _) | (Right x) = Right $ UnpackedOfNotRealsIsNotReal x

||| The full list of available operators is described in the section 11.3 of the IEEE 1800-2023 standard.
||| We deviate from the standard in the following ways:
||| - Assignment operators are not included, they are considered the part of the statements instead.
||| - Ternary conditioanl operator is included dirtectly into the expression type.
public export
data SVBinaryOperator : (lType : SVType) -> (rType : SVType) -> (retType : SVType) -> Type where
  SVLogicAnd : SVIntegral x => SVIntegral y => SVBinaryOperator x y (SVar Logic')
  SVLogicOr : SVIntegral x => SVIntegral y => SVBinaryOperator x y (SVar Logic')
  SVLogicImply : SVIntegral x => SVIntegral y => SVBinaryOperator x y (SVar Logic')
  SVLogicalEquiv : SVIntegral x => SVIntegral y => SVBinaryOperator x y (SVar Logic')
  SVBiteWiseAnd : SVIntegral x => SVIntegral y => SVIntegral z => SVBinaryOperator x y z
  SVBiteWiseOr : SVIntegral x => SVIntegral y => SVIntegral z => SVBinaryOperator x y z
  SVAdd : SVIntegralOrReal x => SVIntegralOrReal y => SVIntegralOrReal z => SVBinaryOperator x y z
  SVSub : SVIntegralOrReal x => SVIntegralOrReal y => SVIntegralOrReal z => SVBinaryOperator x y z
  SVMult: SVIntegralOrReal x => SVIntegralOrReal y => SVIntegralOrReal z => SVBinaryOperator x y z
  SVDiv : SVIntegralOrReal x => SVIntegralOrReal y => SVIntegralOrReal z => SVBinaryOperator x y z
  SVLess : SVIntegralOrReal x => SVIntegralOrReal y => SVBinaryOperator x y (SVar Logic')
  SVLessEq: SVIntegralOrReal x => SVIntegralOrReal y => SVBinaryOperator x y (SVar Logic')
  SVGreater : SVIntegralOrReal x => SVIntegralOrReal y => SVBinaryOperator x y (SVar Logic')
  SVLogicalEqual : SVBinaryOperator x y (SVar Logic')
  SVLogicalNotEqual : SVBinaryOperator x y (SVar Logic')
  SVCaseEqual : NotExtendedReal x => NotExtendedReal y => SVBinaryOperator x y (SVar Bit')
  SVCaseNotEqual : NotExtendedReal x => NotExtendedReal y => SVBinaryOperator x y (SVar Bit')

public export
data SVUnaryOperator : (iType : SVType) -> (oType : SVType) -> Type where
  SVUnaryMinus : SVIntegral t => SVUnaryOperator t t
  SVLogicalNegation : SVIntegralOrReal t => SVUnaryOperator t (SVar Logic')
  SVBitwiseNegation : SVIntegral t => SVUnaryOperator t t

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
data SVExpression : (expressionType : SVType) -> (objects : SVObjList) -> (usedObjects : TreeOfIndices objects) -> Type where
  SVVarOrNet : (i : IndexInObjects objects)-> AtIndexType i t => SVExpression t objects (Leaf i)
  SVLogicConstant : Binary S4 -> SVExpression (SVar Logic') objects EmptyLeaf
  SVBitConstant : Binary S2 -> SVExpression (SVar Bit') objects EmptyLeaf
  SVPackedArrayConstant : TypeLiteralVect (S (max s e `minus` min s e)) t -> PABasic t => SVExpression (PackedArr t s e) objects EmptyLeaf
  SVIndexPackedArray : (i : IndexInObjects objects) ->
                       PABasic t =>
                       AtIndexType i (PackedArr t s e) =>
                       (arrIndex : Nat) ->
                       LTE (min s e) arrIndex => LTE arrIndex (max s e) =>
                       SVExpression t objects (Leaf i)
  SVIndexUnpackedArray : (i : IndexInObjects objects) ->
                         AtIndexType i (UnpackedArr t s e) =>
                         (arrIndex : Nat) ->
                         LTE (min s e) arrIndex => LTE arrIndex (max s e) =>
                         SVExpression t objects (Leaf i)
  SVApplyBinaryOperator : SVBinaryOperator lType rType retType ->
                          (l : SVExpression lType objects usedObjectsL) -> (r : SVExpression rType objects usedObjectsR) ->
                          SVExpression retType objects (Node2Degree usedObjectsL usedObjectsR)
  SVApplyUnaryOperator : SVUnaryOperator iType oType -> SVExpression iType objects usedObjects -> SVExpression oType objects usedObjects
  SVConditionalExpression : SVIntegral cType =>
                            SVExpression cType objects usedPortsCond ->
                            tType `IsEquivalentTo` fType =>
                            SVExpression tType objects usedPortsT ->
                            SVExpression fType objects usedPortsF ->
                            SVExpression rType objects (Node3Degree usedPortsCond usedPortsT usedPortsF)

export
genExpressions : Fuel ->
                 (expressionType : SVType) -> (objects : SVObjList) -> (usedObjects : TreeOfIndices objects) ->
                 Gen MaybeEmpty (SVExpression expressionType objects usedObjects)
