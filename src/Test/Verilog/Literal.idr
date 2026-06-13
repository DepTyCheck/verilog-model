module Test.Verilog.Literal

import Data.Vect

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

namespace BinaryVect

  ||| Length-indexed non-empty list of bits.
  public export
  data BinaryVect : Nat -> State -> Type where
    One  : Binary s -> BinaryVect 1 s
    More : Binary s -> BinaryVect n s -> BinaryVect (S n) s

  public export
  toVect : BinaryVect n s -> Vect n (Binary s)
  toVect (One  x)    = [x]
  toVect (More x xs) = x :: toVect xs

  public export
  length : BinaryVect n s -> Nat
  length (One  _)    = 1
  length (More _ xs) = S $ length xs

  public export %inline
  (.length) : BinaryVect n s -> Nat
  (.length) = length

public export
data BitsList : Nat -> State -> Type where
  Unsized : BinaryList s   -> BitsList n s
  Sized   : BinaryVect n s -> BitsList n s

namespace SVTypeLiteralVect

  public export
  data SVTypeLiteral : SVType -> Type

  public export
  data SVTypeLiteralVect : Nat -> SVType-> Type where
    Nil  : SVTypeLiteralVect 0 t
    (::) : SVTypeLiteral t -> SVTypeLiteralVect n t -> SVTypeLiteralVect (S n) t

  export
  toList : SVTypeLiteralVect l t -> List $ SVTypeLiteral t
  toList []      = []
  toList (x::xs) = x :: toList xs

  public export
  data SVTypeLiteral : SVType -> Type where
    RL  : BitsList (Real.bitsCnt t) S4 -> SVTypeLiteral $ RVar t
    AL  : BitsList 1 (states t) -> SVTypeLiteral $ AVar t
    VL  : BitsList (Vector.bitsCnt t) (states t) -> SVTypeLiteral $ VVar t
    ||| Structure literal
    PAL  : {t : SVType} -> (p : PABasic t) => SVTypeLiteralVect (S $ max s e `minus` min s e) t -> SVTypeLiteral $ PackedArr t s e
    ||| A flat bits list
    PANL : {t : SVType} -> (p : PABasic t) => BitsList (bitsCnt $ PackedArr t s e) (states t) -> SVTypeLiteral $ PackedArr t s e
    UAL  : SVTypeLiteralVect (S $ max s e `minus` min s e) t -> SVTypeLiteral $ UnpackedArr t s e
