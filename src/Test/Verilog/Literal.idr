module Test.Verilog.Literal

import public Test.Verilog.Module

import Data.Fuel

import Test.DepTyCheck.Gen


||| 0 or 1
public export
data SValue2 = Z'  | O'

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

public export
data Binary : Bool -> Type where
  S2 : SValue2 -> Binary True
  S4 : SValue4 -> Binary False

public export
Show (Binary _) where
  show (S2 x) = show x
  show (S4 x) = show x

public export
is2state: SVBasic -> Bool
is2state Logic'   = False
is2state Wire'    = True
is2state Uwire'   = True
is2state Int'     = False
is2state Integer' = True
is2state Bit'     = True
is2state Real'    = False

-- public export
-- bits: SVBasic -> Nat
-- bits Logic'   = 1
-- bits Wire'    = 1
-- bits Uwire'   = 1
-- bits Int'     = 32
-- bits Integer' = 32
-- bits Bit'     = 1
-- bits Real'    = 64

public export
data LiteralList : SVType -> Nat -> Type

public export
data Literal : SVType -> Type where
  Single : Binary (is2state x) -> Literal (Var x)
  PArr   : {a : SVArray t s e} -> LiteralList t (max s e `minus` min s e) -> Literal (Arr a)

public export
data LiteralList : SVType -> Nat -> Type where
  Nil  : LiteralList t 0
  (::) : Literal t -> LiteralList t l -> LiteralList t (S l)

namespace GenLiterals

  public export
  data LiteralList' : PortsList -> Type where
    Nil  : LiteralList' t
    (::) : Literal t -> LiteralList' sk -> LiteralList' (t :: sk)

export
genLiterals : Fuel -> (sk: PortsList) -> Gen MaybeEmpty $ LiteralList' sk
