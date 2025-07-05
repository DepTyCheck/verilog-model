module Test.Verilog.Literal

import public Test.Verilog.SVType

import Data.Fuel

import Test.DepTyCheck.Gen
import Test.DepTyCheck.Gen.Coverage

%default total

||| Single bit binary literal
public export
data Binary : State -> Type where
  B2 : S2Value -> Binary S2
  B4 : S4Value -> Binary S4

export
genBinary : Fuel -> (s : State) -> Gen MaybeEmpty $ Binary s

namespace BinaryVect

  public export
  data BinaryVect : Nat -> State -> Type where
    Nil  : BinaryVect 0 s
    (::) : Binary s -> BinaryVect n s -> BinaryVect (S n) s
  
  genBinVect' : Fuel -> (n : Nat) -> (s : State) -> Gen MaybeEmpty $ BinaryVect n s
  genBinVect' x 0 s     = pure []
  genBinVect' x (S k) s = do
    b <- genBinary x s
    rest <- genBinVect' x k s
    pure $ b::rest
  
  export
  genBinVect : Fuel -> (n : Nat) -> (s : State) -> Gen MaybeEmpty $ BinaryVect n s
  genBinVect x n s = withCoverage $ genBinVect' x n s

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
    RL  : BinaryVect 1 S4 -> TypeLiteral $ RVar t
    SL  : BinaryVect 1 (states t) -> TypeLiteral $ SVar t
    VL  : BinaryVect 1 (states t) -> TypeLiteral $ VVar t
    PAL : {t : SVType} -> (p : PABasic t) => BinaryVect 1 (states t) -> TypeLiteral $ PackedArr t s e -- (bitsCnt $ PackedArr t s e)
    UAL : TypeLiteralVect (S $ max s e `minus` min s e) t -> TypeLiteral $ UnpackedArr t s e

namespace LiteralsList

  public export
  data LiteralsList : SVObjList -> Type where
    Nil  : LiteralsList []
    (::) : TypeLiteral (valueOf obj) -> LiteralsList rest -> LiteralsList $ obj::rest

export
genLiterals : Fuel -> (sk : SVObjList) -> 
              (Fuel -> (n : Nat) -> (s : State) -> Gen MaybeEmpty $ BinaryVect n s) =>
              Gen MaybeEmpty $ LiteralsList sk
