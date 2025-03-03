module Test.Verilog.Literal

import public Test.Verilog.Module

import Data.Fuel

import Test.DepTyCheck.Gen

%default total

||| 0 or 1
public export
data SValue2 = Z' | O'

public export
Show SValue2 where
  show Z' = "0"
  show O' = "1"

||| 0 or 1 or x or z
public export
data SValue4 = Z'' | O'' | X | H

public export
Show SValue4 where
  show Z'' = "0"
  show O'' = "1"
  show X   = "x"
  show H   = "z"

||| Single bit binary literal
public export
data BitState : Bool -> Type where
  S2 : SValue2 -> BitState True
  S4 : SValue4 -> BitState False

public export
Show (BitState _) where
  show (S2 x) = show x
  show (S4 x) = show x

public export
is2state : SVBasic -> Bool
is2state Logic'   = False
is2state Wire'    = True
is2state Uwire'   = True
is2state Int'     = False
is2state Integer' = True
is2state Bit'     = True
is2state Real'    = False

||| List of binary literals
public export
data BinaryList : SVType -> Nat -> Type

||| Multi-bit binary literal
public export
data Binary : SVType -> Type where
  Single : BitState (is2state x) -> Binary (Var x)
  UArr   : BinaryList t (S $ max s e `minus` min s e) -> Binary (Arr $ Unpacked t s e)
  PArr   : BinaryList t (S $ max s e `minus` min s e) -> Binary (Arr $ Packed   t s e @{_})

public export
data BinaryList : SVType -> Nat -> Type where
  Nil  : BinaryList t 0
  (::) : Binary t -> BinaryList t l -> BinaryList t (S l)

public export
toList : BinaryList t l -> List $ Binary t
toList []        = []
toList (x :: xs) = x :: toList xs

public export
data LiteralsList : PortsList -> Type where
  Empty : LiteralsList t
  Cons  : (t: SVType) -> Binary t -> LiteralsList sk -> LiteralsList (t :: sk)

export
genLiterals : Fuel -> (sk: PortsList) -> Gen MaybeEmpty $ LiteralsList sk
