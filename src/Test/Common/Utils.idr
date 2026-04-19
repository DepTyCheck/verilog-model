module Test.Common.Utils

import Data.Vect
import public Data.Fin

%default total

namespace FinsList

  public export
  data FinsList : Nat -> Type where
    Nil  : FinsList n
    (::) : Fin n -> FinsList n -> FinsList n

  %name FinsList fs

  public export
  (.asList) : FinsList n -> List $ Fin n
  (.asList) []      = []
  (.asList) (x::xs) = x :: xs.asList

  public export
  (.length) : FinsList n -> Nat
  (.length) []      = 0
  (.length) (x::xs) = S xs.length

  public export
  index : (fs : FinsList s) -> Fin fs.length -> Fin s
  index (f::_ ) FZ     = f
  index (_::fs) (FS i) = index fs i

  public export
  data FinNotIn : FinsList srcs -> Fin srcs -> Type where
    FNIEmpty : FinNotIn [] f
    FNICons  : {x, f : Fin srcs} -> (0 _ : So $ x /= f) -> (fni: FinNotIn xs f) -> FinNotIn (x :: xs) f

  public export
  toVect : (fs : FinsList n) -> Vect (fs.length) (Fin n)
  toVect []        = []
  toVect (x :: xs) = x :: toVect xs

  public export
  lookUp : (fl : FinsList n) -> Fin (fl.length) -> Fin n
  lookUp = index

  public export
  listLookUp : (y : FinsList n) -> FinsList (y.length) -> FinsList n
  listLookUp xs []        = []
  listLookUp xs (y :: ys) = lookUp xs y :: listLookUp xs ys

  -- public export
  -- data FinInFL : FinsList l -> Fin l -> Type where
  --   Here  : (n, n' : Fin l) => (0 _ : So $ n == n') => FinInFL (n::ns) n'
  --   There : (n, n' : Fin l) => (0 _ : So $ n /= n') => FinInFL ns n' -> FinInFL (n::ns) n'

namespace MFin

  public export
  data MFin : Nat -> Type where
    Nothing : MFin n
    Just    : Fin n -> MFin n
