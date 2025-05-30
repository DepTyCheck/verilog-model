module Test.Verilog.Assign

import public Test.Verilog.SVType

import Data.Fuel
import Data.Vect
import Data.Fin
import Data.Nat

import Test.DepTyCheck.Gen
import Test.DepTyCheck.Gen.Coverage

%default total

namespace SD

  -- public export
  -- data SingleDrivenAssigns : FinsList n -> Type
  public export
  data FinNotInSD : {n : Nat} -> {fins : FinsList n} -> SingleDrivenAssigns fins -> Fin (fins.length) -> Type

  ||| 10.3.2
  ||| Variables can only be driven by one continuous assignment or by one primitive
  ||| output or module output. It shall be an error for a variable driven by a continuous assignment or output to
  ||| have an initializer in the declaration or any procedural assignment.
  ||| IEEE 1800-2023
  |||
  ||| Assigns to sinks without sources (some of them can also be multidriven)
  public export
  data SingleDrivenAssigns : FinsList n -> Type where
    Empty : SingleDrivenAssigns fins
    Cons  : {n : Nat} -> {fins : FinsList n} -> 
            (f : Fin fins.length) -> (rest : SingleDrivenAssigns fins) -> FinNotInSD rest f -> SingleDrivenAssigns fins

  export
  genSingleDriven : Fuel -> {n : Nat} -> (fins : FinsList n) ->
    -- (Fuel -> {n': Nat} -> {fins': FinsList n'} -> (rest': SingleDrivenAssigns fins') -> (f': Fin $ fins'.length) ->
    -- Gen MaybeEmpty $ FinNotInSD rest' f') =>
    Gen MaybeEmpty $ SingleDrivenAssigns fins

  public export
  toList : {sk : SVObjList} -> {fins : FinsList $ sk.length} -> SingleDrivenAssigns fins -> List $ Fin $ fins.length
  toList Empty           = []
  toList (Cons f rest _) = f :: toList rest

  export
  genFNE : Fuel -> {n : Nat} -> (f, fin : Fin $ n) -> Gen MaybeEmpty $ NotEqFin f fin

  public export
  data FinNotInSD : {n : Nat} -> {fins : FinsList n} -> (rest : SingleDrivenAssigns fins) -> (f : Fin (fins.length)) -> Type where
    FNIEmpty : FinNotInSD Empty f
    FNICons  : {rest : SingleDrivenAssigns fins} -> {f, fin : Fin (fins.length)} -> {fni : FinNotInSD rest f} ->
      (0 _ : NotEqFin f fin) ->
      (npi : FinNotInSD rest fin) -> FinNotInSD (Cons f rest fni) fin

  -- genFINSD' : Fuel -> {n' : Nat} -> {fins' : FinsList n'} -> (rest' : SingleDrivenAssigns fins') -> (f' : Fin (fins'.length)) ->
  --           Gen MaybeEmpty $ FinNotInSD rest' f'
  -- genFINSD' x Empty           _   = pure FNIEmpty
  -- genFINSD' x (Cons f rest z) fin = do
  --   res <- genFINSD' x rest fin
  --   fne <- genFNE x f fin
  --   pure $ FNICons fne res

  -- export
  -- genFINSD : Fuel -> {n' : Nat} -> {fins' : FinsList n'} -> (rest' : SingleDrivenAssigns fins') -> (f' : Fin (fins'.length)) ->
  --           Gen MaybeEmpty $ FinNotInSD rest' f'
  -- genFINSD x rest f = withCoverage $ genFINSD' x rest f

namespace MD
  ||| All resolved nets are multidriven
  ||| TODO: Add necessary constructors when extend SVBasic
  public export
  data Multidriven : SVType -> Type where
    W : Multidriven $ Var Wire'
    U : Multidriven t -> Multidriven $ Arr $ Unpacked t {}
    P : Multidriven t -> Multidriven $ Arr $ Packed   t {} @{_}

  ||| If the port is not declared as either top input or top output,
  ||| then this predicate should be used to indicate whether the connection to the specified port is multidriven or not
  public export
  data NoTopDeclMD : SVType -> Type where
    ||| Vars and packed arrs aren't explicitly declared, so they are interpreted as the default_net_type (which is multidriven)
    VP : VarOrPacked t -> NoTopDeclMD t
    ||| The port type itself also could be multidriven itself
    MD : Multidriven t -> NoTopDeclMD $ Arr $ Unpacked t {}

  ||| All submodule inputs connected to given source are multidriven (never declared or unpacked & md)
  public export
  data AllSIsMD : (sk : PortsList) -> (subConns : MFinsList ss) -> (f : Fin ss) -> Type where
    ASISEmpty : AllSIsMD [] [] f
    ASISHere  : {c : Maybe $ Fin ss} -> EqMaybeF c f    -> NoTopDeclMD p -> (rest : AllSIsMD ps cs f) -> AllSIsMD (p::ps) (c::cs) f
    ASISTHere : {c : Maybe $ Fin ss} -> NotEqMaybeF c f                  -> (rest : AllSIsMD ps cs f) -> AllSIsMD (p::ps) (c::cs) f

  ||| A source is multidriven if there is no explicit declaration of its type as singledriven
  public export
  data MultidrivenSource : (ss : PortsList) -> (tIs : Nat) ->
                           (subInPs : PortsList) -> (subConns : MFinsList ss.length) -> (topConns : MFinsList ss.length) ->
                           (f : Fin ss.length) -> Type where
    ||| Connection to a top input is multidriven if the top input port has multidriven type
    TopInp : GT tIs (finToNat f) -> Multidriven (typeOf ss f) -> MultidrivenSource ss tIs subInPs subConns topConns f
    ||| Connection to a submodule output is multidriven if
    ||| 1. it's not connected to a top output port
    ||| 2. all connected submodule inputs are multidriven
    ||| 3. the source has no explicit decl or multidriven itself
    NoDecl : LTE tIs (finToNat f) -> FinNotInMFL topConns f -> AllSIsMD subInPs subConns f -> NoTopDeclMD (typeOf ss f) ->
             MultidrivenSource ss tIs subInPs subConns topConns f

  

  ||| An index of a multidriven sink
  public export
  data MultidrivenSink : (subInPs : PortsList) -> (subConns : MFinsList ss) -> (topOuPs : PortsList) -> Type where
    ||| The sink is not declared and has no source
    NoSource : (f : Fin subInPs.length) -> EqMaybeMFMF Nothing (find subConns f) -> NoTopDeclMD (typeOf subInPs f) -> MultidrivenSink subInPs subConns topOuPs
    ||| The sink is top output and multidriven itself
    TopOut   : (f : Fin topOuPs.length) -> Multidriven (typeOf topOuPs f) -> MultidrivenSink subInPs subConns topOuPs

  ||| 10.3.2
  ||| Nets can be driven by multiple continuous assignments or by a mixture of primitive outputs, module outputs,
  ||| and continuous assignments.
  ||| IEEE 1800-2023
  |||
  ||| Each implicit connection has a net type by default. So all implicit connections and explicit net connections can be multidriven
  ||| Some sinks are not connected to the source and vice versa
  public export
  data MultiDrivenAssigns : (ss : PortsList) -> (tIs : Nat) ->
                            (subInPs : PortsList) -> (subConns : MFinsList ss.length) ->
                            (topOuPs : PortsList) -> (topConns : MFinsList ss.length) -> Type where
    Empty      : MultiDrivenAssigns ss tIs subInPs subConns topOuPs topConns
    ConsSource : (f : Fin ss.length) -> MultidrivenSource ss tIs subInPs subConns topConns f ->
                 (rest : MultiDrivenAssigns ss tIs subInPs subConns topOuPs topConns) -> MultiDrivenAssigns ss tIs subInPs subConns topOuPs topConns
    ConsSink   : (f : MultidrivenSink subInPs subConns topOuPs) ->
                 (rest : MultiDrivenAssigns ss tIs subInPs subConns topOuPs topConns) -> MultiDrivenAssigns ss tIs subInPs subConns topOuPs topConns

  -- public export
  -- toListSSs : {ss : PortsList} -> {subConns : MFinsList ss.length} -> {topConns : MFinsList ss.length} ->
  --             MultiDrivenAssigns ss tIs subInPs subConns topOuPs topConns -> List $ Fin $ ss.length
  -- toListSSs Empty                 = []
  -- toListSSs (ConsSource f _ rest) = f :: toListSSs rest
  -- toListSSs (ConsSink   f   rest) = toListSSs rest

  -- public export
  -- toListSkSbInps : {ss : PortsList} -> {subConns : MFinsList ss.length} -> {topConns : MFinsList ss.length} ->
  --                  MultiDrivenAssigns ss tIs subInPs subConns topOuPs topConns -> List $ Fin $ subInPs.length
  -- toListSkSbInps Empty                 = []
  -- toListSkSbInps (ConsSource f _ rest) = toListSkSbInps rest
  -- toListSkSbInps (ConsSink   f   rest) = case f of
  --   NoSource f' _ _ => f' :: toListSkSbInps rest
  --   TopOut   _  _   => toListSkSbInps rest

  -- public export
  -- toListSkTopOuts : {ss : PortsList} -> {subConns : MFinsList ss.length} -> {topConns : MFinsList ss.length} ->
  --                   MultiDrivenAssigns ss tIs subInPs subConns topOuPs topConns -> List $ Fin $ topOuPs.length
  -- toListSkTopOuts Empty                 = []
  -- toListSkTopOuts (ConsSource f _ rest) = toListSkTopOuts rest
  -- toListSkTopOuts (ConsSink   f   rest) = case f of
  --   NoSource _  _ _ => toListSkTopOuts rest
  --   TopOut   f' _   => f' :: toListSkTopOuts rest

export
genMultiDriven : Fuel -> (ss : PortsList) -> (tIs : Nat) ->
                 (subInPs : PortsList) -> (subConns : MFinsList ss.length) ->
                 (topOuPs : PortsList) -> (topConns : MFinsList ss.length) ->
                 Gen MaybeEmpty $ MultiDrivenAssigns ss tIs subInPs subConns topOuPs topConns
