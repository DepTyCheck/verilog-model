module Test.Verilog.Assign

import public Test.Verilog.Module

import Data.Fuel
import Data.Vect

import Test.DepTyCheck.Gen
import Test.DepTyCheck.Gen.Coverage

%default total

namespace SD

  public export
  data SingleDrivenAssigns : FinsList n -> Type
  public export
  data FinNotInSD : {n: Nat} -> {fins: FinsList n} -> SingleDrivenAssigns fins -> Fin (fins.length) -> Type

  public export
  data SingleDrivenAssigns : FinsList n -> Type where
    Empty : SingleDrivenAssigns fins
    Cons  : {n: Nat} -> {fins: FinsList n} -> (f: Fin fins.length) -> (rest: SingleDrivenAssigns fins) -> FinNotInSD rest f -> SingleDrivenAssigns fins

  export
  genSingleDriven : Fuel -> {n: Nat} -> (fins: FinsList n) ->
    (Fuel -> {n': Nat} -> {fins': FinsList n'} -> (rest': SingleDrivenAssigns fins') -> (f': Fin $ fins'.length) ->
    Gen MaybeEmpty $ FinNotInSD rest' f') =>
    Gen MaybeEmpty $ SingleDrivenAssigns fins


  public export
  toList : {sk: PortsList} -> {fins: FinsList $ sk.length} -> SingleDrivenAssigns fins -> List $ Fin $ fins.length
  toList Empty           = []
  toList (Cons f rest _) = f :: toList rest

  public export
  data FinNotEqual : Fin n -> Fin n -> Type where
    ZS  : FinNotEqual FZ (FS i)
    SZ  : FinNotEqual (FS i) FZ
    Rec : FinNotEqual x y -> FinNotEqual (FS x) (FS y)

  export
  genFNE: Fuel -> {n: Nat} -> (f, fin : Fin $ n) -> Gen MaybeEmpty $ FinNotEqual f fin

  public export
  data FinNotInSD : {n: Nat} -> {fins: FinsList n} -> (rest: SingleDrivenAssigns fins) -> (f: Fin (fins.length)) -> Type where
    FNIEmpty : FinNotInSD Empty f
    FNICons  : {rest: SingleDrivenAssigns fins} -> {f, fin : Fin (fins.length)} -> {fni: FinNotInSD rest f} ->
      (0 _ : FinNotEqual f fin) ->
      (npi: FinNotInSD rest fin) -> FinNotInSD (Cons f rest fni) fin

  genFINSD' : Fuel -> {n': Nat} -> {fins': FinsList n'} -> (rest': SingleDrivenAssigns fins') -> (f': Fin (fins'.length)) ->
            Gen MaybeEmpty $ FinNotInSD rest' f'
  genFINSD' x Empty           _   = pure FNIEmpty
  genFINSD' x (Cons f rest z) fin = do
    res <- genFINSD' x rest fin
    fne <- genFNE x f fin
    pure $ FNICons fne res

  export
  genFINSD : Fuel -> {n': Nat} -> {fins': FinsList n'} -> (rest': SingleDrivenAssigns fins') -> (f': Fin (fins'.length)) ->
            Gen MaybeEmpty $ FinNotInSD rest' f'
  genFINSD x rest f = withCoverage $ genFINSD' x rest f

namespace MD

  ||| All net types except uwire are multidriven
  ||| TODO: Add necessary constructors when extend SVBasic
  public export
  data Multidriven : SVType -> Type where
    W : Multidriven $ Var Wire'
    U : Multidriven t -> Multidriven $ Arr $ Unpacked t {}
    P : Multidriven t -> Multidriven $ Arr $ Packed   t {} @{_}

  public export
  data MultidrivenAt : (sk: PortsList) -> Fin (sk.length) -> Type where
    MHere  : Multidriven   p    -> MultidrivenAt (p::ps) FZ
    MThere : MultidrivenAt ps i -> MultidrivenAt (p::ps) (FS i)

  public export
  data MultiDrivenAssigns : PortsList -> Type where
    Empty : MultiDrivenAssigns fins
    Cons  : (f: Fin sk.length) -> (rest: MultiDrivenAssigns sk) -> MultidrivenAt sk f -> MultiDrivenAssigns sk

  export
  genMultiDriven : Fuel -> (sk: PortsList) -> Gen MaybeEmpty $ MultiDrivenAssigns sk

  public export
  toList : {sk: PortsList} -> MultiDrivenAssigns sk -> List $ Fin $ sk.length
  toList Empty           = []
  toList (Cons f rest _) = f :: toList rest
