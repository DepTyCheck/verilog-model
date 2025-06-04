module Test.Verilog.Literal

import public Test.Verilog.SVType

import Data.Fuel

import Test.DepTyCheck.Gen
import Test.DepTyCheck.Gen.Coverage

%default total

namespace Logic2

  public export
  data SValue2 = Z | S

  public export
  Show SValue2 where
    show Z = "0"
    show S = "1"

||| 6.3.1 Logic values
||| 
||| The SystemVerilog value set consists of the following four basic values:
||| 0—represents a logic zero or a false condition
||| 1—represents a logic one or a true condition
||| x—represents an unknown logic value
||| z—represents a high-impedance state
||| 
||| IEEE 1800-2023
namespace Logic4
  ||| 0 or 1 or x or z
  public export
  data SValue4 = Z | S | X | H

  public export
  Show SValue4 where
    show Z = "0"
    show S = "1"
    show X = "x"
    show H = "z"

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
is2state : SVType -> Bool
is2state (RVar x)             = True
is2state (SVar Bit')          = False
is2state (SVar Logic')        = True
is2state (SVar Reg')          = True
is2state (VVar Byte')         = False
is2state (VVar Shortint')     = False
is2state (VVar Int')          = False
is2state (VVar Longint')      = False
is2state (VVar Integer')      = True
is2state (VVar Time')         = True
is2state (PackedArr    t s e) = is2state t
is2state (UnpackedArr  t s e) = is2state t

||| List of binary literals
public export
data BinaryList : SVType -> Nat -> Type

||| Multi-bit binary literal
public export
data Binary : SVType -> Type where
  Single : BitState (is2state svt) -> Binary svt
  UArr   : BinaryList t (S $ max s e `minus` min s e) -> Binary (UnpackedArr t s e)
  PArr   : BinaryList t (S $ max s e `minus` min s e) -> (p : PABasic t) => Binary (PackedArr   t s e)

public export
data BinaryList : SVType -> Nat -> Type where
  Nil  : BinaryList t 0
  (::) : Binary t -> BinaryList t l -> BinaryList t (S l)

public export
toList : BinaryList t l -> List $ Binary t
toList []        = []
toList (x :: xs) = x :: toList xs

namespace Literals

  public export
  data LiteralsList : SVObjList -> Type where
    Nil  : LiteralsList []
    (::) : Binary (valueOf t) -> LiteralsList sk -> LiteralsList (t :: sk)

genBinary' : Fuel -> (t : SVType) -> Gen MaybeEmpty $ Binary t

export
genSingleBit : Fuel -> (b : Bool) -> Gen MaybeEmpty $ BitState b

genBinaryList : Fuel -> (t : SVType) -> (n: Nat) -> Gen MaybeEmpty $ BinaryList t n
genBinaryList x t Z = pure Nil
genBinaryList x t (S n) = do
  rest <- genBinaryList x t n
  bin <- genBinary' x t
  pure $ bin :: rest

genBinary' x (PackedArr t s e) = do
  lst <- genBinaryList x t $ S $ max s e `minus` min s e
  pure $ PArr lst 
genBinary' x (UnpackedArr t s e) = do
  lst <- genBinaryList x t $ S $ max s e `minus` min s e
  pure $ UArr lst
genBinary' x y = do
  bit <- genSingleBit x (is2state y)
  pure $ Single bit

genBinary : Fuel -> (t : SVType) -> Gen MaybeEmpty $ Binary t
genBinary x t = withCoverage $ genBinary' x t

genLiterals' : Fuel -> (sk: SVObjList) -> Gen MaybeEmpty $ LiteralsList sk
genLiterals' _ []      = pure []
genLiterals' x (y::ys) = do 
  bin <- genBinary' x (valueOf y)
  rest <- genLiterals' x ys
  pure $ bin :: rest

export
genLiterals : Fuel -> (sk: SVObjList) -> Gen MaybeEmpty $ LiteralsList sk
genLiterals x sk = withCoverage $ genLiterals' x sk
