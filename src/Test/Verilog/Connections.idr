module Test.Verilog.Connections

import public Test.Verilog.SVType

import Data.Fuel
import Data.Fin

import Test.DepTyCheck.Gen
import Test.DepTyCheck.Gen.Coverage

%default total

namespace ConnectionsValidation

--   ||| Returns the size of packed array.
--   ||| But the actual number of bits that a type stores may be different (Var SVBasic represents types of different sizes)
--   public export
--   packedSize : SVType -> Nat
--   packedSize (Var _)                = 1
--   packedSize (Arr $ Unpacked t _ _) = packedSize t
--   packedSize (Arr $ Packed   t s e) = S (max s e `minus` min s e) * packedSize t

--   ||| Checks if two ports have the same basic type
--   |||
--   ||| Example. `EqSuperBasic` states that `a1` and `b1` share the same basic type (bit). 
--   ||| This is one of the conditions that make the connection between modules `a` and `b` valid:
--   ||| module a(output bit [9:0] a1 [3:0]);
--   ||| endmodule: a
--   ||| 
--   ||| module b(input bit [2:0][5:0] b1 [3:0]);
--   ||| endmodule: b
--   public export
--   data EqSuperBasic : SVType -> SVType -> Type where
--     EqBasicV : EqSVBasic    t t' -> EqSuperBasic (Var t)                    (Var t')
--     EqBasicP : EqSuperBasic t t' -> EqSuperBasic (Arr $ Packed   t {} @{_}) (Arr $ Packed   t' {} @{_})
--     EqBasicU : EqSuperBasic t t' -> EqSuperBasic (Arr $ Unpacked t {})      (Arr $ Unpacked t' {})

--   ||| Checks if two unpacked arrays have the same size.
--   |||
--   ||| Example. `EqUnpackedArrSig` states that `a1` and `b1` have the same size (3 - 0) + (2 - 0) = 5. 
--   ||| This is one of the conditions that make the connection between modules `a` and `b` valid:
--   ||| module a(output bit [9:0] a1 [3:0][0:2]);
--   ||| endmodule: a
--   ||| 
--   ||| module b(input bit [2:0][5:0] b1 [3:0][2:0]);
--   ||| endmodule: b
--   public export
--   data EqUnpackedArrSig : SVType -> SVType -> Type where
--     Other  : VarOrPacked t -> VarOrPacked t' -> EqUnpackedArrSig t t'
--     EqUArr : EqUnpackedArrSig t t' -> EqNat (max s e + min s' e') (max s' e' + min s e) ->
--       EqUnpackedArrSig (Arr $ Unpacked t s e) (Arr $ Unpacked t' s' e')

--   public export
--   data CanConnect : SVType -> SVType -> Type where
--     CCVarOrPacked : VarOrPacked p1 -> VarOrPacked p2 -> CanConnect p1 p2
--     ||| 6.22.2 Equivalent types
--     ||| d) Unpacked fixed-size array types are equivalent if they have equivalent element types and equal size.
--     |||
--     ||| IEEE 1800 - 2023
--     CCUnpackedUnpacked : EqSuperBasic t t' -> EqNat (packedSize t) (packedSize t') ->
--       EqUnpackedArrSig (Arr $ Unpacked t s e) (Arr $ Unpacked t' s' e') -> CanConnect (Arr $ Unpacked t s e) (Arr $ Unpacked t' s' e')

  ||| The list of sources may be empty (Nil). In this case, either an implicit net is declared or an external net declaration must exist
  |||
  ||| 6.10 Implicit declarations
  ||| If an identifier is used in a port expression declaration,
  ||| then an implicit net of default net type shall be assumed, with the vector width of the port expression declaration.
  public export
  data SourceForSink : (srcs : SVTList) -> (sink : SVType) -> Type
--     NoSource  : SourceForSink srcs sink
--     HasSource : (srcIdx : Fin $ length srcs) -> CanConnect (typeOf srcs srcIdx) sink -> SourceForSink srcs sink

-- namespace ConnsList

  public export
  data Connections : (srcs, sinks : SVTList) -> (topOuts : Bool) -> (tIs : Nat) -> Type

  public export
  data NoSourceConns : SourceForSink srcs sink' -> Connections srcs sinks topOuts tIs -> Type

  ||| Each output maybe has connection from some input.
  ||| If topOuts then each input can go to one output. Otherwise each input can go to several outputs
  public export
  data Connections : (srcs, sinks : SVTList) -> (topOuts : Bool) -> (tIs : Nat) -> Type where -- change bool to mode
    Empty : Connections srcs [] t tIs
    Cons  : {srcs : SVTList} -> {sink : SVType} ->  (sfs : SourceForSink srcs sink) -> (rest : Connections srcs sinks t tIs) -> 
            {nsc : NoSourceConns sfs rest} -> Connections srcs (sink :: sinks) t tIs
  
--   public export
--   connsToMFL : Connections srcs sinks t tIs -> MFinsList (srcs.length)
--   connsToMFL Empty                            = []
--   connsToMFL (Cons NoSource             rest) = Nothing :: connsToMFL rest
--   connsToMFL (Cons (HasSource srcIdx _) rest) = Just srcIdx :: connsToMFL rest

--   ||| List of source indexes
--   public export
--   consToFins : Connections srcs sinks t tIs -> FinsList (srcs.length)
--   consToFins Empty                              = []
--   consToFins (Cons NoSource             rest) = consToFins rest
--   consToFins (Cons (HasSource srcIdx _) rest) = srcIdx :: consToFins rest

--   ||| If Connections are indexed as Unique, then source indexes must not repeat
--   public export
--   data NoSourceConns : (sfs : SourceForSink srcs sink') -> (conns : Connections srcs sinks topOuts tIs) -> Type where
--     NotUnique : {conns : Connections srcs sinks False tIs} -> NoSourceConns sfs conns
--     ConsNoS   : {conns : Connections srcs sinks True  tIs} -> NoSourceConns NoSource conns
--     ConsHasS  : {conns : Connections srcs sinks True  tIs} -> FinNotIn (consToFins conns) f -> NoSourceConns (HasSource f cc) conns

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
    -- (sssi : Connections (m.inputs ++ allOutputs {ms} subMs) (allInputs {ms} subMs) False m.inpsCount) ->
    -- (ssto : Connections (m.inputs ++ allOutputs {ms} subMs) (m.outputs)            True  m.inpsCount) ->
    (cont : Modules (m::ms)) ->
    Modules ms

-- export
-- genNotEqFin : Fuel -> {n : Nat} -> (a, b : Fin n) -> Gen MaybeEmpty $ NotEqFin a b
-- export
-- genSourceForSink : Fuel -> (srcs : PortsList) -> (sink' : SVType) -> Gen MaybeEmpty $ SourceForSink srcs sink'

-- genFinNotIn' : Fuel -> {srcs : Nat} -> (fins : FinsList srcs) -> (fin : Fin srcs) -> Gen MaybeEmpty $ FinNotIn fins fin
-- genFinNotIn' x []        fin = pure FNIEmpty
-- genFinNotIn' x (f :: fs) fin = do
--   rest <- genFinNotIn' x fs fin
--   ne <- genNotEqFin x f fin
--   pure $ FNICons ne rest

-- genFinNotIn : Fuel -> {srcs : Nat} -> (fins : FinsList srcs) -> (fin : Fin srcs) -> Gen MaybeEmpty $ FinNotIn fins fin
-- genFinNotIn x fins fin  = withCoverage $ genFinNotIn' x fins fin

-- genNoSourceConns' : Fuel -> {topOuts : Bool} -> {srcs : PortsList} ->
--                    (sfs : SourceForSink srcs sink) -> (conns : Connections srcs sinks topOuts tIs) -> Gen MaybeEmpty $ NoSourceConns sfs conns
-- genNoSourceConns' x {topOuts = False} sfs conns = pure NotUnique
-- genNoSourceConns' x {topOuts = True} NoSource conns = pure ConsNoS
-- genNoSourceConns' x {topOuts = True} (HasSource srcIdx y) conns = do
--   fni <- genFinNotIn x (consToFins conns) srcIdx
--   pure $ ConsHasS fni

-- genNoSourceConns : Fuel -> {topOuts : Bool} -> {srcs : PortsList} ->
--                    (sfs : SourceForSink srcs sink) -> (conns : Connections srcs sinks topOuts tIs) -> Gen MaybeEmpty $ NoSourceConns sfs conns
-- genNoSourceConns x sfs conns = withCoverage $ genNoSourceConns' x sfs conns

-- genConnections' : Fuel -> (srcs : PortsList) -> (sinks : PortsList) -> (topOuts : Bool) -> (tIs : Nat) -> 
--                  Gen MaybeEmpty $ Connections srcs sinks topOuts tIs
-- genConnections' x srcs []        t tIs = pure Empty
-- genConnections' x srcs (y :: ys) t tIs = do
--   sfs <- genSourceForSink x srcs y
--   rest <- genConnections' x srcs ys t tIs
--   nsc <- genNoSourceConns x sfs rest
--   pure $ Cons sfs rest {nsc}

-- export
-- genConnections : Fuel -> (srcs : PortsList) -> (sinks : PortsList) -> (topOuts : Bool) -> (tIs : Nat) -> 
--                  Gen MaybeEmpty $ Connections srcs sinks topOuts tIs
-- genConnections x srcs sinks t tIs = withCoverage $ genConnections' x srcs sinks t tIs

-- export
-- genModules : Fuel -> (ms : ModuleSigsList) ->
--   (Fuel -> (srcs : PortsList) -> (sink' : SVType) -> Gen MaybeEmpty $ SourceForSink srcs sink') =>
--   (Fuel -> (srcs' : PortsList) -> (sinks' : PortsList) -> (topOuts' : Bool) -> (tIs' : Nat) -> 
--   Gen MaybeEmpty $ Connections srcs' sinks' topOuts' tIs') =>
--   Gen MaybeEmpty $ Modules ms
