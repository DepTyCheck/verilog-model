module Test.Common.UniqueFins

import Data.Fuel
import Test.DepTyCheck.Gen

import public Test.Common.Utils

public export
data UniqueFins : (n : Nat) -> (fs : FinsList n) -> Type where
  Nil  : UniqueFins n []
  (::) : (f : Fin n) -> FinNotIn rest f => UniqueFins n rest -> UniqueFins n (f::rest)

export
genUF : Fuel -> (n : Nat) -> Gen MaybeEmpty (fs : FinsList n ** UniqueFins n fs)
