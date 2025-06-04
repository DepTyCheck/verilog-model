module Test.Verilog.Connections

import public Test.Verilog.SVType

import Data.Fuel
import Data.Fin

import Test.DepTyCheck.Gen
import Test.DepTyCheck.Gen.Coverage

%default total

||| TopOuts are sinks, SubInps are sources
public export
data ConnMode = TopOuts | SubInps

||| 6.22.2 Equivalent types
||| c) Packed arrays, packed structures, packed unions, and built-in integral types are equivalent if they
||| contain the same number of total bits, are either all 2-state or all 4-state, and are either all signed or
||| all unsigned.
||| NOTE — If any bit of a packed structure or union is 4-state, the entire structure or union is considered 4-state.
public export
data EquivalentSVT : SVType -> SVType -> Type where
  ESVT : So (bitsCnt t == bitsCnt t') -> So (states t == states t') -> So (isSigned t == isSigned t') -> EquivalentSVT t t'

||| Checks if two ports have the same basic type
|||
||| Example. `EqSuperBasic` states that `a1` and `b1` share the same basic type (bit). 
||| This is one of the conditions that make the connection between modules `a` and `b` valid:
||| module a(output bit [9:0] a1 [3:0]);
||| endmodule: a
||| 
||| module b(input bit [2:0][5:0] b1 [3:0]);
||| endmodule: b
public export
data EqSuperBasic : SVType -> SVType -> Type where
  VV : VarOrPacked t -> VarOrPacked t' -> EquivalentSVT t t' -> EqSuperBasic t t'
  UU : EqSuperBasic t t' -> EqSuperBasic (UnpackedArr t s e) (UnpackedArr t' s' e')

||| Checks if two unpacked arrays have the same size.
|||
||| Example. `EqUnpackedArrSig` states that `a1` and `b1` have the same size (3 - 0) + (2 - 0) = 5. 
||| This is one of the conditions that make the connection between modules `a` and `b` valid:
||| module a(output bit [9:0] a1 [3:0][0:2]);
||| endmodule: a
||| 
||| module b(input bit [2:0][5:0] b1 [3:0][2:0]);
||| endmodule: b
public export
data EqUnpackedArrSig : SVType -> SVType -> Type where
  Other  : VarOrPacked t -> VarOrPacked t' -> EqUnpackedArrSig t t'
  EqUArr : EqUnpackedArrSig t t' -> So ((max s e + min s' e') == (max s' e' + min s e)) ->
    EqUnpackedArrSig (UnpackedArr t s e) (UnpackedArr t' s' e')

public export
data CanConnect : SVType -> SVType -> Type where
  CCVarOrPacked : VarOrPacked p1 -> VarOrPacked p2 -> CanConnect p1 p2
  ||| 6.22.2 Equivalent types
  ||| d) Unpacked fixed-size array types are equivalent if they have equivalent element types and equal size.
  |||
  ||| IEEE 1800 - 2023
  CCUnpackedUnpacked : IsUnpackedArr t -> IsUnpackedArr t' ->
    EqSuperBasic t t' -> EqUnpackedArrSig t t' ->
    CanConnect t t'

||| The list of sources may be empty (Nil). In this case, either an implicit net is declared or an external net declaration must exist
|||
||| 6.10 Implicit declarations
||| If an identifier is used in the terminal list of a primitive instance or in the port connection list of a
||| module, interface, program, or static checker instance (but not a procedural checker instance, see
||| 17.3), and that identifier has not been declared previously in the scope where the instantiation
||| appears or in any scope whose declarations can be directly referenced from the scope where the
||| instantiation appears (see 23.9), then an implicit scalar net of default net type shall be assumed.
public export
data SourceForSink : (srcs : SVObjList) -> (sink : SVObject) -> (srcIdx : MFin srcs.length) -> Type where
  NoSource  : SourceForSink srcs sink Nothing
  HasSource : (srcIdx : Fin $ length srcs) -> CanConnect (valueOf $ typeOf srcs srcIdx) (valueOf sink) -> SourceForSink srcs sink $ Just srcIdx

public export
data Connections : (srcs, sinks : SVObjList) -> (cm : ConnMode) -> MFinsList (srcs.length) -> Type

public export
data NoSourceConns : {srcs : SVObjList} -> MFin srcs.length -> 
                     {ids : MFinsList $ srcs.length} -> Connections srcs sinks cm ids -> Type

||| Each output maybe has connection from some input.
||| If topOuts then each input can go to one output. Otherwise each input can go to several outputs
public export
data Connections : (srcs, sinks : SVObjList) -> (cm : ConnMode) -> MFinsList (srcs.length) -> Type where
  Empty : Connections srcs [] cm []
  Cons  : {srcs : SVObjList} -> {srcIdx : MFin srcs.length} -> {ids : MFinsList $ srcs.length} ->
          SourceForSink srcs sink srcIdx -> (rest : Connections srcs sinks cm ids) -> 
          {nsc : NoSourceConns srcIdx rest} -> Connections srcs (sink :: sinks) cm (srcIdx::ids)

||| If Connections are indexed as Unique, then source indexes must not repeat
public export
data NoSourceConns : {srcs : SVObjList} -> MFin srcs.length -> 
                     {ids : MFinsList $ srcs.length} -> Connections srcs sinks cm ids -> Type where
  NotUnique : {conns : Connections srcs sinks SubInps ids} -> NoSourceConns sfs conns
  ConsNoS   : {conns : Connections srcs sinks TopOuts ids} -> NoSourceConns Nothing conns
  ConsHasS  : {conns : Connections srcs sinks TopOuts ids} -> FinNotInMFL ids srcIdx -> NoSourceConns (Just srcIdx) conns

||| 3.2 Design elements
|||
||| A design element is a:
||| - module (see Clause 23)
||| - program (see Clause 24)
||| - interface (see Clause 25)
||| - checker (see Clause 17)
||| - package (see Clause 26)
||| - primitive (see Clause 28)
||| - configuration (see Clause 33).
|||
||| 3.3 Modules
||| Some of the constructs that modules can contain include the following:
||| — Ports, with port declarations
||| — Data declarations, such as nets, variables, structures, and unions
||| — Constant declarations
||| — User-defined type definitions
||| — Class definitions
||| — Imports of declarations from packages
||| — Subroutine definitions
||| — Instantiations of other modules, programs, interfaces, checkers, and primitives
||| — Instantiations of class objects
||| — Continuous assignments
||| — Procedural blocks
||| — Generate blocks
||| — Specify blocks
|||
||| IEEE 1800-2023
public export
data Modules : ModuleSigsList -> Type where

  End : Modules ms

  ||| A module containing only submodules and connections.
  NewCompositeModule :
    (m : ModuleSig) ->
    (subMs : FinsList ms.length) ->
    -- Remember: Do not change the concatenation order of the port lists, the many features depend on it (search for m.inpsCount and tIs usages)
    {sicons : MFinsList $ (m.inputs ++ allOutputs {ms} subMs).length} ->
    {tocons : MFinsList $ (m.inputs ++ allOutputs {ms} subMs).length} ->
    (sssi : Connections (m.inputs ++ allOutputs {ms} subMs) (allInputs {ms} subMs) SubInps sicons) ->
    (ssto : Connections (m.inputs ++ allOutputs {ms} subMs) (m.outputs)            TopOuts tocons) ->
    (cont : Modules (m::ms)) ->
    Modules ms


export
genMF : Fuel -> (srcs : Nat) -> Gen MaybeEmpty $ MFin srcs
export
genCC : Fuel -> (t,t' : SVType) -> Gen MaybeEmpty $ CanConnect t t'
export
genFNI : Fuel -> {srcs : Nat } -> (ids : MFinsList srcs) -> (y : Fin srcs) -> Gen MaybeEmpty $ FinNotInMFL ids y

genNSC : Fuel -> {srcs : SVObjList} -> (srcIdx : MFin srcs.length) -> {ids : MFinsList $ srcs.length} -> {cm : ConnMode} -> (rest : Connections srcs sinks cm ids) -> 
         Gen MaybeEmpty $ NoSourceConns srcIdx rest
genNSC x src      {cm = SubInps} rest = pure NotUnique
genNSC x Nothing  {cm = TopOuts} rest = pure ConsNoS
genNSC x (Just y) {cm = TopOuts} rest = do
  fni <- genFNI x ids y
  pure $ ConsHasS fni

genConns' : Fuel -> (srcs' : SVObjList) -> (sinks' : SVObjList) -> (cm' : ConnMode) -> 
            Gen MaybeEmpty $ (cons' : MFinsList (srcs'.length) ** Connections srcs' sinks' cm' cons')
genConns' x srcs []              cm = pure ([] ** Empty)
genConns' x srcs (sink :: sinks) cm = do
  (cons ** rest) <- genConns' x srcs sinks cm
  mf <- genMF x srcs.length
  nsc <- genNSC x mf rest
  case mf of
    Nothing       => pure (mf::cons ** Cons NoSource {nsc} rest)
    (Just srcIdx) => do
      cc <- genCC x (valueOf $ typeOf srcs srcIdx) (valueOf sink)
      pure (mf::cons ** Cons (HasSource srcIdx cc) {nsc} rest)

-- Only genConns is actually used as an external generator
export
genConns : Fuel -> (srcs' : SVObjList) -> (sinks' : SVObjList) -> (cm' : ConnMode) -> 
           Gen MaybeEmpty $ (cons' : MFinsList (srcs'.length) ** Connections srcs' sinks' cm' cons')
genConns x srcs sinks cm = withCoverage $ genConns' x srcs sinks cm

export
genModules : Fuel -> (ms : ModuleSigsList) ->
  (Fuel -> (srcs' : SVObjList) -> (sinks' : SVObjList) -> (cm' : ConnMode) -> 
  Gen MaybeEmpty $ (cons' : MFinsList (srcs'.length) ** Connections srcs' sinks' cm' cons')) =>
  Gen MaybeEmpty $ Modules ms
