module Test.Common.Utils

import Data.Vect
import public Data.Fin

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
  fromVect : Vect l (Fin sk) -> FinsList sk
  fromVect []      = []
  fromVect (x::xs) = x :: fromVect xs

  public export
  fromList : List (Fin n) -> FinsList n
  fromList []      = []
  fromList (x::xs) = x :: fromList xs

  public export
  mapFin : (Fin n -> Fin (S n)) -> FinsList n -> FinsList (S n)
  mapFin f Nil = Nil
  mapFin f (x :: xs) = f x :: mapFin f xs

  public export
  allFins : (n : Nat) -> FinsList n
  allFins Z     = []
  allFins (S n) = FZ :: mapFin FS (allFins n)

  public export
  data FinNotIn : FinsList srcs -> Fin srcs -> Type where
    FNIEmpty : FinNotIn [] f
    FNICons  : {x, f : Fin srcs} -> (0 _ : So $ x /= f) -> (fni: FinNotIn xs f) -> FinNotIn (x :: xs) f

namespace MFinsList

  public export
  data MFin : Nat -> Type where
    Nothing : MFin n
    Just    : Fin n -> MFin n
  
  public export
  f2mf : Fin a -> MFin a
  f2mf f = Just f
  
  public export
  data NotEqMaybeF : MFin a -> Fin a -> Type where
    NEqMFN : NotEqMaybeF Nothing n
    NEqMFJ : {m, n : Fin a} -> (0 _ : So $ m /= n) -> NotEqMaybeF (Just m) n

  public export
  data EqMaybeF : MFin a -> Fin a -> Type where
    EqMF : EqMaybeF (Just n) n

  public export
  data EqMFMF : MFin a -> MFin a -> Type where
    EqMFMF' : EqMFMF n n
  
  public export
  fromMaybe : Maybe (Fin n) -> MFin n
  fromMaybe Nothing  = Nothing
  fromMaybe (Just x) = Just x

  public export
  data MFinsList : Nat -> Nat -> Type where
    Nil  : MFinsList 0 n
    (::) : MFin n -> MFinsList l n -> MFinsList (S l) n

  %name MFinsList fs

  public export
  (.length) : MFinsList l n -> Nat
  (.length) []      = 0
  (.length) (x::xs) = S xs.length
  
  public export
  toList : MFinsList l n -> List $ MFin n
  toList []      = []
  toList (x::xs) = x :: toList xs

  public export
  toVect : MFinsList l n -> Vect l $ MFin n
  toVect []      = []
  toVect (x::xs) = x :: toVect xs

  public export
  find : (ms : MFinsList l n) -> Fin l -> MFin n
  find (m::_ ) FZ     = m
  find (_::ms) (FS i) = find ms i

  public export
  toMFL : Vect l (Maybe $ Fin r) -> MFinsList l r
  toMFL []      = []
  toMFL (x::xs) = fromMaybe x :: toMFL xs

  public export
  data FinNotInMFL : (conns : MFinsList l ss) -> (f : Fin ss) -> Type where
    Empty : FinNotInMFL [] f
    Cons  : {c : MFin ss} -> NotEqMaybeF c f -> (rest : FinNotInMFL cs f) -> FinNotInMFL (c::cs) f
  
  public export
  data FinInMFL : MFinsList l ss -> Fin ss -> Type where
    Here  : EqMaybeF  n n'   => FinInMFL (n::ns) n'
    There : NotEqMaybeF n n' => FinInMFL ns n' -> FinInMFL (n::ns) n'
