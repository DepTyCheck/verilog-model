module Test.Common.Utils

import Data.Vect
import public Data.Fin
import public Data.List
import public Data.List.Ext

import public Syntax.IHateParens.List

%default total

namespace FinsList

  public export
  FinsList : Nat -> Type
  FinsList n = List (Fin n)

  %name FinsList fs

  public export
  data FinNotIn : FinsList srcs -> Fin srcs -> Type where
    FNIEmpty : FinNotIn [] f
    FNICons  : {x, f : Fin srcs} -> (0 _ : So $ x /= f) -> (fni: FinNotIn xs f) -> FinNotIn (x :: xs) f

namespace MFinsList

  public export
  MFin : Nat -> Type
  MFin n = Maybe (Fin n)
