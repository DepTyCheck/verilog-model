module Test.Verilog.Literal

import public Test.Verilog.SVType

%default total

||| 6.3.1 Logic values
|||
||| The SystemVerilog value set consists of the following four basic values:
||| 0—represents a logic zero or a false condition
||| 1—represents a logic one or a true condition
||| x—represents an unknown logic value
||| z—represents a high-impedance state
|||
||| IEEE 1800-2023
public export
data Binary : State -> Type where
  S : Binary a
  Z : Binary a
  X : Binary S4
  H : Binary S4

namespace BinaryList

  public export
  data BinaryList : State -> Type where
    One  : Binary s -> BinaryList s
    More : Binary s -> BinaryList s -> BinaryList s

namespace TypeLiteralVect

  public export
  data TypeLiteral : SVType -> Type

  public export
  data TypeLiteralVect : Nat -> SVType-> Type where
    Nil  : TypeLiteralVect 0 t
    (::) : TypeLiteral t -> TypeLiteralVect n t -> TypeLiteralVect (S n) t

  export
  toList : TypeLiteralVect l t -> List $ TypeLiteral t
  toList []      = []
  toList (x::xs) = x :: toList xs

  public export
  data TypeLiteral : SVType -> Type where
    RL   : BinaryList S4 -> TypeLiteral $ RVar t
    AL   : BinaryList (states t) -> TypeLiteral $ AVar t
    VL   : BinaryList (states t) -> TypeLiteral $ VVar t
    PALF : (p : PABasic t) => TypeLiteralVect (S $ max s e `minus` min s e) t -> TypeLiteral $ PackedArr t (One $ MkPair s e)
    PALM : (p : PABasic t) => TypeLiteralVect (S $ max s e `minus` min s e) (PackedArr t shape) ->
           TypeLiteral $ PackedArr t (MkPair s e `More` shape)
    UALF : (p : VarOrPacked t) => TypeLiteralVect (S $ max s e `minus` min s e) t -> TypeLiteral $ UnpackedArr t (One $ MkPair s e)
    UALM : {shape : ArrayShape} -> (p : VarOrPacked t) => TypeLiteralVect (S $ max s e `minus` min s e) (UnpackedArr t shape) ->
           TypeLiteral $ UnpackedArr t (MkPair s e `More` shape)
