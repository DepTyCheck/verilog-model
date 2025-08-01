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

public export
basicIntegral : SVType -> Bool
basicIntegral (RVar x)            = False
basicIntegral (SVar x)            = True
basicIntegral (VVar x)            = True
basicIntegral (PackedArr   t _ _) = True
basicIntegral (UnpackedArr x _ _) = basicIntegral x

||| 6.22.2 Equivalent types
||| c) Packed arrays, packed structures, packed unions, and built-in integral types are equivalent if they
||| contain the same number of total bits, are either all 2-state or all 4-state, and are either all signed or
||| all unsigned.
||| NOTE — If any bit of a packed structure or union is 4-state, the entire structure or union is considered 4-state.
public export
data EquivalentSVT : SVType -> SVType -> Type where
  ESVT : So (bitsCnt t == bitsCnt t') -> So (states t == states t') -> So (isSigned t == isSigned t') -> So (basicIntegral t == basicIntegral t') ->
         EquivalentSVT t t'

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

namespace PortListAliases

  public export
  topSnks : (m : ModuleSig) -> SVObjList
  topSnks m = m.outputs

  public export
  topSnks' : (m : ModuleSig) -> Nat
  topSnks' m = m.outsCount

  public export
  subSnks : (ms : ModuleSigsList) -> (m : ModuleSig) -> (subMs : FinsList ms.length) -> SVObjList
  subSnks ms m subMs = allInputs {ms} subMs

  public export
  subSnks' : (ms : ModuleSigsList) -> (m : ModuleSig) -> (subMs : FinsList ms.length) -> Nat
  subSnks' ms m subMs = length $ subSnks ms m subMs

  public export
  topSrcs : (m : ModuleSig) -> SVObjList
  topSrcs m = m.inputs

  public export
  topSrcs' : (m : ModuleSig) ->  Nat
  topSrcs' m = m.inpsCount

  public export
  subSrcs : (ms : ModuleSigsList) -> (m : ModuleSig) -> (subMs : FinsList ms.length) -> SVObjList
  subSrcs ms m subMs = allOutputs {ms} subMs

  public export
  subSrcs' : (ms : ModuleSigsList) -> (m : ModuleSig) -> (subMs : FinsList ms.length) -> Nat
  subSrcs' ms m subMs = length $ subSrcs ms m subMs

namespace GreatLookUp

  public export
  lookUp : (fl : FinsList n) -> Fin (fl.length) -> Fin n
  lookUp fl f = index fl f

public export
data SubRefsList : (ports : SVObjList) -> (fl : FinsList $ ports.length) -> FinsList (fl.length) -> Type where
  ACCNil  : SubRefsList ports use []
  ACCOne  : {use : _} -> (i : Fin use.length) -> SubRefsList ports use [i]
  ACCCons : {use : _} -> (i : Fin use.length) -> (j : Fin use.length) -> {rest : _} ->  {_: FinNotIn (j :: rest) i} ->
            {_ : CanConnect (valueOf $ typeOf ports $ lookUp use i) (valueOf $ typeOf ports $ lookUp use j)} ->
            SubRefsList ports use (j :: rest) ->
            SubRefsList ports use (i :: j :: rest)

public export
extractFins : SubRefsList sp suf newFins -> FinsList (suf.length)
extractFins ACCNil             = []
extractFins (ACCOne i)         = [i]
extractFins (ACCCons i _ rest) = i::extractFins rest

public export
data PortRef : (topPorts : SVObjList) -> (topUseFins : FinsList $ length topPorts) -> 
               (subPorts : SVObjList) -> (subUseFins : FinsList $ length subPorts) -> Type where
  Top : (i : Fin tuf.length) -> PortRef tp tuf sp suf
  Sub : SubRefsList sp suf newFins -> PortRef tp tuf sp suf

export
isUnconnected : PortRef tp tuf sp suf -> Bool
isUnconnected (Top _)               = False
isUnconnected (Sub ACCNil)          = True
isUnconnected (Sub (ACCOne _))      = False
isUnconnected (Sub (ACCCons _ _ _)) = False

namespace UseFins

  public export
  data UseFins : (ms : ModuleSigsList) -> (m : ModuleSig) -> (subMs : FinsList ms.length) -> Type where
    UF : FinsList (topSnks' m) -> FinsList (subSnks' ms m subMs) -> FinsList (topSrcs' m) -> FinsList (subSrcs' ms m subMs) -> 
          UseFins ms m subMs

  -- public export
  -- Eq (UseFins ms m subMs) where
  --   (==) (UF tsk ssk tsc ssc) (UF tsk' ssk' tsc' ssc') = tsk == tsk' && ssk == ssk' && tsc == tsc' && ssc == ssc'
  
  -- public export
  -- DecEq (UseFins ms m subMs) where
  --   decEq (UF tsk1 ssk1 tsc1 ssc1) (UF tsk2 ssk2 tsc2 ssc2) =
  --     case decEq tsk1 tsk2 of
  --       Yes Refl =>
  --         case decEq ssk1 ssk2 of
  --           Yes Refl =>
  --             case decEq tsc1 tsc2 of
  --               Yes Refl =>
  --                 case decEq ssc1 ssc2 of
  --                   Yes Refl => Yes Refl
  --                   No contra => No (\Refl => contra Refl)
  --               No contra => No (\Refl => contra Refl)
  --           No contra => No (\Refl => contra Refl)
  --       No contra => No (\Refl => contra Refl)
  
  public export
  minus : UseFins ms m subMs -> UseFins ms m subMs -> UseFins ms m subMs
  minus (UF tsk ssk tsc ssc) (UF tsk' ssk' tsc' ssc') = UF (minus tsk tsk') (minus ssk ssk') (minus tsc tsc') (minus ssc ssc')

  -- public export
  -- data MinusUF : UseFins ms m subMs -> UseFins ms m subMs -> UseFins ms m subMs -> Type where
  --   MUF : MinusUF (UF tsk ssk tsc ssc) (UF tsk' ssk' tsc' ssc') $ UF (minus tsk tsk') (minus ssk ssk') (minus tsc tsc') (minus ssc ssc')

  public export
  nilUF : UseFins ms m subMs
  nilUF = UF [] [] [] []

  public export
  fullUF : {ms : _} -> {m : _} -> {subMs : _} -> UseFins ms m subMs
  fullUF {ms} {m} {subMs} = UF (allFins $ topSnks' m) (allFins $ subSnks' ms m subMs) (allFins $ topSrcs' m) (allFins $ subSrcs' ms m subMs)

  -- public export
  -- data UFFull : UseFins ms m subMs -> Type where
  --   Yes : UFFull $ UseFins.fullUF
  --   No  : {x : UseFins ms m subMs} -> {_ : So $ x /= UseFins.fullUF} -> UFFull x
  
  -- public export
  -- ufFullView : {ms : _} -> {m : _} -> {subMs : _} -> (uf : UseFins ms m subMs) -> UFFull uf
  -- ufFullView (UF (allFins $ topSnks' m) (allFins $ subSnks' ms m subMs) (allFins $ topSrcs' m) (allFins $ subSrcs' ms m subMs)) = ?njklm
  -- ufFullView _ = ?knjl

public export
data NonNilPF : PortRef tp tuf sp suf -> Type where
  NNT : NonNilPF $ Top i
  NNO : NonNilPF $ Sub $ ACCOne i
  NNC : NonNilPF $ Sub $ ACCCons i j rest

public export
data AnySubPort : PortRef tp tuf sp suf -> SVObject -> Type where
  ASPO : AnySubPort (Sub {tp=tp} {tuf=tuf} {sp=sp} {suf=suf} $ ACCOne i) (typeOf sp $ lookUp suf i)
  ASPC : AnySubPort (Sub {tp=tp} {tuf=tuf} {sp=sp} {suf=suf} $ ACCCons i j rest) (typeOf sp $ lookUp suf i)

public export
data ConnectSinkSource : PortRef tpsk utsk spsk ussk -> PortRef tpsc utsc spsc ussc -> Type where
  TS : {utsk : FinsList tpsk.length} -> {utsc : FinsList tpsc.length} -> {i : Fin utsk.length} ->
       {_ : AnySubPort (Sub {tp=tpsc} {tuf = utsc} {sp = spsc} {suf = ussc} refs) subT} ->
       {_ : CanConnect (valueOf $ typeOf tpsk $ lookUp utsk i) (valueOf subT)} ->
       ConnectSinkSource (Top {tp=tpsk} {tuf = utsk} {sp = spsk} {suf = ussk} i) (Sub {tp=tpsc} {tuf = utsc} {sp = spsc} {suf = ussc} refs)
  ST : {utsk : FinsList tpsk.length} -> {utsc : FinsList tpsc.length} -> {i : Fin utsc.length} ->
       {_ : AnySubPort (Sub {tp=tpsk} {tuf = utsk} {sp = spsk} {suf = ussk} refs) subT} ->
       {_ : CanConnect (valueOf subT) (valueOf $ typeOf tpsc $ lookUp utsc i)} ->
       ConnectSinkSource (Sub {tp=tpsk} {tuf = utsk} {sp = spsk} {suf = ussk} refs) (Top {tp=tpsc} {tuf = utsc} {sp = spsc} {suf = ussc} i)
  SS : {utsk : FinsList tpsk.length} -> {utsc : FinsList tpsc.length} ->
       AnySubPort (Sub {tp=tpsk} {tuf = utsk} {sp = spsk} {suf = ussk} refs) subT ->
       {_ : AnySubPort (Sub {tp=tpsc} {tuf = utsc} {sp = spsc} {suf = ussc} refs') subT'} ->
       {_ : CanConnect (valueOf subT) (valueOf subT')} ->
       ConnectSinkSource (Sub {tp=tpsk} {tuf = utsk} {sp = spsk} {suf = ussk} refs) (Sub {tp=tpsc} {tuf = utsc} {sp = spsc} {suf = ussc} refs')
  NA : {utsk : FinsList tpsk.length} ->
       {nonNilSource : PortRef tpsc utsc spsc ussc} -> NonNilPF nonNilSource -> 
       ConnectSinkSource (Sub {tp=tpsk} {tuf = utsk} {sp = spsk} {suf = ussk} ACCNil) nonNilSource
  AN : {utsc : FinsList tpsc.length} ->
       {nonNilSink : PortRef tpsk utsk spsk ussk} -> NonNilPF nonNilSink -> 
       ConnectSinkSource nonNilSink (Sub {tp=tpsc} {tuf = utsc} {sp = spsc} {suf = ussc} ACCNil)

public export
refsToFins : {0 sp : SVObjList} -> {suf : FinsList $ sp.length} -> {0 newFins : FinsList $ suf.length} ->
             SubRefsList sp suf newFins -> FinsList (sp.length)
refsToFins x = fromList $ map (lookUp suf) $ (extractFins x).asList

public export
extractUFsk : {tsk : _} -> {ssk :_} -> PortRef (topSnks m) tsk (subSnks ms m subMs) ssk -> UseFins ms m subMs
extractUFsk {tsk} (Top i) = UF [lookUp tsk i] [] [] []
extractUFsk {ssk} (Sub x) = UF [] (refsToFins x) [] []

public export
extractUFsc : {tsc : _} -> {ssc :_} -> PortRef (topSrcs m) tsc (subSrcs ms m subMs) ssc -> UseFins ms m subMs
extractUFsc {tsc} (Top i) = UF [] [] [lookUp tsc i] []
extractUFsc {ssc} (Sub x) = UF [] [] [] (refsToFins x)

-- public export
-- data DropUF : UseFins ms m subMs -> {utsk : _} -> {ussk : _} -> PortRef (topSnks m) utsk (subSnks ms m subMs) ussk -> 
--               {utsc : _} -> {ussc : _} -> PortRef (topSrcs m) utsc (subSrcs ms m subMs) ussc -> UseFins ms m subMs -> Type where
--   DUF : MinusUF uf (extractUFsk sk) ufNoSk -> MinusUF ufNoSk (extractUFsc sc) ufNoSkSc -> DropUF uf sk sc ufNoSkSc
public export
dropUF : UseFins ms m subMs -> 
         {utsk : _} -> {ussk : _} -> PortRef (topSnks m) utsk (subSnks ms m subMs) ussk -> 
         {utsc : _} -> {ussc : _} -> PortRef (topSrcs m) utsc (subSrcs ms m subMs) ussc -> UseFins ms m subMs
dropUF uf sk sc = minus (minus uf $ extractUFsk sk) $ extractUFsc sc

-- public export
-- data MultiConnection : (ms : ModuleSigsList) -> (m : ModuleSig) -> (subMs : FinsList ms.length) -> 
--                        (preUF : UseFins ms m subMs) -> Type where
--   MC : (sk : PortRef (topSnks m) tsk (subSnks ms m subMs) ssk) -> (sc : PortRef (topSrcs m) tsc (subSrcs ms m subMs) ssc) -> ConnectSinkSource sk sc -> 
--        MultiConnection ms m subMs (UF tsk ssk tsc ssc) -- $ dropUF (UF tsk ssk tsc ssc) sk sc

public export
data MultiConnectionVect : (n : Nat) -> (ms : ModuleSigsList) -> (m : ModuleSig) -> (subMs : FinsList ms.length) -> 
                           (preUF : UseFins ms m subMs) -> Type where
  Empty : MultiConnectionVect 0 ms m subMs UseFins.nilUF
  Cons : {ms : ModuleSigsList} -> {m : ModuleSig} -> {subMs : FinsList ms.length} ->
         {tsk : FinsList $ length $ topSnks m} -> {ssk : FinsList $ length $ subSnks ms m subMs} ->
         (sk : PortRef (topSnks m) tsk (subSnks ms m subMs) ssk) -> 
         {tsc : FinsList $ length $ topSrcs m} -> {ssc : FinsList $ length $ subSrcs ms m subMs} ->
         (sc : PortRef (topSrcs m) tsc (subSrcs ms m subMs) ssc) -> 
         ConnectSinkSource sk sc -> 
         {pre : UseFins ms m subMs} ->
         MultiConnectionVect n ms m subMs (dropUF pre sk sc) -> MultiConnectionVect (S n) ms m subMs pre

public export
data ShortConn : (ms : ModuleSigsList) -> (m : ModuleSig) -> (subMs : FinsList ms.length) -> Type where
  MkSC : {tsk : FinsList $ length $ topSnks m} -> {ssk : FinsList $ length $ subSnks ms m subMs} -> 
         (sk : PortRef (topSnks m) tsk (subSnks ms m subMs) ssk) -> 
         {tsc : FinsList $ length $ topSrcs m} -> {ssc : FinsList $ length $ subSrcs ms m subMs} ->
         (sc : PortRef (topSrcs m) tsc (subSrcs ms m subMs) ssc) -> ShortConn ms m subMs

export
toVect : MultiConnectionVect n ms m subMs uf -> Vect n $ ShortConn ms m subMs
toVect Empty               = []
toVect (Cons sk sc _ rest) = MkSC sk sc :: toVect rest

public export
length : MultiConnectionVect n ms m subMs uf -> Nat
length Empty               = 0
length (Cons sk sc _ rest) = S $ length rest

export
unpOrDefault : (munp : SVObject) -> SVObject
unpOrDefault munp = if isUnpacked munp then munp else defaultNetType

export
typeOf : MultiConnectionVect n ms m subMs uf -> Fin n -> SVObject
typeOf (Cons _  _  _ rest) (FS i)                                                       = typeOf rest i
typeOf (Cons      {m}         {tsk} (Top i)               (Sub _)               _ _) FZ = typeOf (m.outputs) $ lookUp tsk i
typeOf (Cons      {m}         {tsc} (Sub _)               (Top i)               _ _) FZ = typeOf (m.inputs) $ lookUp tsc i
typeOf (Cons {ms} {m} {subMs} {ssk} (Sub (ACCOne  i))     (Sub _)               _ _) FZ = unpOrDefault $ typeOf (subSnks ms m subMs) $ lookUp ssk i
typeOf (Cons {ms} {m} {subMs} {ssk} (Sub (ACCCons i _ _)) (Sub _)               _ _) FZ = unpOrDefault $ typeOf (subSnks ms m subMs) $ lookUp ssk i
typeOf (Cons {ms} {m} {subMs} {ssc} (Sub ACCNil)          (Sub (ACCOne i))      _ _) FZ = unpOrDefault $ typeOf (subSrcs ms m subMs) $ lookUp ssc i
typeOf (Cons {ms} {m} {subMs} {ssc} (Sub ACCNil)          (Sub (ACCCons i _ _)) _ _) FZ = unpOrDefault $ typeOf (subSrcs ms m subMs) $ lookUp ssc i
typeOf _ _ = defaultNetType     -- actually there are no missing cases (-_-)

-- public export
-- data Connections : (srcs, sinks : SVObjList) -> (cm : ConnMode) -> MFinsList (sinks.length) (srcs.length) -> Type

-- public export
-- data NoSourceConns : {srcs : SVObjList} -> MFin srcs.length -> 
--                      {ids : MFinsList (sinks.length) (srcs.length)} -> Connections srcs sinks cm ids -> Type

-- ||| Each output maybe has connection from some input.
-- ||| If topOuts then each input can go to one output. Otherwise each input can go to several outputs
-- public export
-- data Connections : (srcs, sinks : SVObjList) -> (cm : ConnMode) -> MFinsList (sinks.length) (srcs.length) -> Type where
--   Empty : Connections srcs [] cm []
--   Cons  : {srcs : SVObjList} -> {srcIdx : MFin srcs.length} -> {ids : MFinsList (sinks.length) (srcs.length)} ->
--           SourceForSink srcs sink srcIdx -> (rest : Connections srcs sinks cm ids) -> 
--           {nsc : NoSourceConns srcIdx rest} -> Connections srcs (sink :: sinks) cm (srcIdx::ids)

-- ||| If Connections are indexed as Unique, then source indexes must not repeat
-- public export
-- data NoSourceConns : {srcs : SVObjList} -> MFin srcs.length -> 
--                      {ids : MFinsList (sinks.length) (srcs.length)} -> Connections srcs sinks cm ids -> Type where
--   NotUnique : {conns : Connections srcs sinks SubInps ids} -> NoSourceConns sfs conns
--   ConsNoS   : {conns : Connections srcs sinks TopOuts ids} -> NoSourceConns Nothing conns
--   ConsHasS  : {conns : Connections srcs sinks TopOuts ids} -> FinNotInMFL ids srcIdx -> NoSourceConns (Just srcIdx) conns

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
    -- -- Remember: Do not change the concatenation order of the port lists, the many features depend on it (search for m.inpsCount and tIs usages)
    -- {sicons : MFinsList (totalInputs {ms} subMs) $ allSrcsLen m ms subMs} ->
    -- {tocons : MFinsList (m.outsCount)            $ allSrcsLen m ms subMs} ->
    -- (sssi : Connections (allSrcs m ms subMs) (allInputs {ms} subMs) SubInps sicons) ->
    -- (ssto : Connections (allSrcs m ms subMs) (m.outputs)            TopOuts tocons) ->
    {n : _} ->
    (mcs : MultiConnectionVect n ms m subMs UseFins.fullUF) ->
    (cont : Modules (m::ms)) ->
    Modules ms


-- export
-- genMF : Fuel -> (srcs : Nat) -> Gen MaybeEmpty $ MFin srcs
-- export
-- genCC : Fuel -> (t,t' : SVType) -> Gen MaybeEmpty $ CanConnect t t'
-- export
-- genFNI : Fuel -> {srcs : Nat} -> {sinks : Nat} -> (ids : MFinsList sinks srcs) -> (y : Fin srcs) -> Gen MaybeEmpty $ FinNotInMFL ids y

-- genNSC : Fuel -> {srcs : SVObjList} -> {sinks : SVObjList} -> (srcIdx : MFin srcs.length) -> 
--          {ids : MFinsList (sinks.length) (srcs.length)} -> {cm : ConnMode} -> (rest : Connections srcs sinks cm ids) -> 
--          Gen MaybeEmpty $ NoSourceConns srcIdx rest
-- genNSC x src      {cm = SubInps} rest = pure NotUnique
-- genNSC x Nothing  {cm = TopOuts} rest = pure ConsNoS
-- genNSC x (Just y) {cm = TopOuts} rest = do
--   fni <- genFNI x ids y
--   pure $ ConsHasS fni

-- genConns' : Fuel -> (srcs' : SVObjList) -> (sinks' : SVObjList) -> (cm' : ConnMode) -> 
--             Gen MaybeEmpty $ (cons' : MFinsList (sinks'.length) (srcs'.length) ** Connections srcs' sinks' cm' cons')
-- genConns' x srcs []              cm = pure ([] ** Empty)
-- genConns' x srcs (sink :: sinks) cm = do
--   (cons ** rest) <- genConns' x srcs sinks cm
--   mf <- genMF x srcs.length
--   nsc <- genNSC x mf rest
--   case mf of
--     Nothing       => pure (mf::cons ** Cons NoSource {nsc} rest)
--     (Just srcIdx) => do
--       cc <- genCC x (valueOf $ typeOf srcs srcIdx) (valueOf sink)
--       pure (mf::cons ** Cons (HasSource srcIdx cc) {nsc} rest)

-- export
-- genConns : Fuel -> (srcs' : SVObjList) -> (sinks' : SVObjList) -> (cm' : ConnMode) -> 
--            Gen MaybeEmpty $ (cons' : MFinsList (sinks'.length) (srcs'.length) ** Connections srcs' sinks' cm' cons')
-- genConns x srcs sinks cm = withCoverage $ genConns' x srcs sinks cm

-- genMC : Fuel -> (ms : ModuleSigsList) -> (m : ModuleSig) -> (subMs : FinsList ms.length) -> 
--         (preUF : UseFins ms m subMs) -> Gen MaybeEmpty $ (aftUF : UseFins ms m subMs ** MultiConnection ms m subMs uf)

export
genPf : Fuel -> (topPorts : SVObjList) -> (topUseFins : FinsList $ length topPorts) -> 
        (subPorts : SVObjList) -> (subUseFins : FinsList $ length subPorts) -> Gen MaybeEmpty $ PortRef topPorts topUseFins subPorts subUseFins
export
genCSS : Fuel -> (sk : PortRef tpsk utsk spsk ussk) -> (sc : PortRef tpsc utsc spsc ussc) -> Gen MaybeEmpty $ ConnectSinkSource sk sc

genMCL' : Fuel -> (ms' : ModuleSigsList) -> (m' : ModuleSig) -> (subMs' : FinsList ms'.length) -> 
          (uf' : UseFins ms' m' subMs') -> Gen MaybeEmpty (n : Nat ** MultiConnectionVect n ms' m' subMs' uf')
genMCL' x ms m subMs (UF []  []  []  [])  = pure $ (0 ** Empty)
genMCL' x ms m subMs (UF tsk ssk tsc ssc) = do
  sk <- genPf x (topSnks m) tsk (subSnks ms m subMs) ssk
  sc <- genPf x (topSrcs m) tsc (subSrcs ms m subMs) ssc
  css <- genCSS x sk sc
  (n ** rest) <- assert_total $ genMCL' x ms m subMs $ dropUF (UF tsk ssk tsc ssc) sk sc
  pure $ ((S n) ** Cons sk sc css rest)

export
genMCL : Fuel -> (ms' : ModuleSigsList) -> (m' : ModuleSig) -> (subMs' : FinsList ms'.length) -> 
         (uf' : UseFins ms' m' subMs') -> Gen MaybeEmpty (n : Nat ** MultiConnectionVect n ms' m' subMs' uf')
genMCL x ms m subMs uf = withCoverage $ genMCL' x ms m subMs uf

export
genModules : Fuel -> (ms : ModuleSigsList) ->
  (Fuel -> (ms' : ModuleSigsList) -> (m' : ModuleSig) -> (subMs' : FinsList ms'.length) -> 
  (uf' : UseFins ms' m' subMs') -> Gen MaybeEmpty (n : Nat ** MultiConnectionVect n ms' m' subMs' uf')) =>
  Gen MaybeEmpty $ Modules ms
