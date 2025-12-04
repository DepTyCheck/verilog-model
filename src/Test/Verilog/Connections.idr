module Test.Verilog.Connections

import public Test.Verilog.SVType

import Data.Fuel
import Data.Fin
import Data.Vect

import Test.DepTyCheck.Gen
import Test.DepTyCheck.Gen.Coverage

%default total

namespace ModuleSig

  public export
  record ModuleSig where
    constructor MkModuleSig
    inputs  : SVObjList
    outputs : SVObjList

  public export
  (.inpsCount) : ModuleSig -> Nat
  (.inpsCount) m = length m.inputs

  public export
  (.outsCount) : ModuleSig -> Nat
  (.outsCount) m = length m.outputs

  %name ModuleSig m

  public export
  data ModuleSigsList = Nil | (::) ModuleSig ModuleSigsList

  %name ModuleSigsList ms

  public export
  length : ModuleSigsList -> Nat
  length []      = Z
  length (_::ms) = S $ length ms

  public export %inline
  (.length) : ModuleSigsList -> Nat
  (.length) = length

  public export
  index : (ms : ModuleSigsList) -> Fin ms.length -> ModuleSig
  index (m::_ ) FZ     = m
  index (_::ms) (FS i) = index ms i

  public export
  (++) : ModuleSigsList -> ModuleSigsList -> ModuleSigsList
  Nil       ++ ys = ys
  (x :: xs) ++ ys = x :: (xs ++ ys)

  public export
  fromList : List ModuleSig -> ModuleSigsList
  fromList [] = []
  fromList (x :: xs) = x :: fromList xs

  public export
  toList : ModuleSigsList -> List ModuleSig
  toList []        = []
  toList (m :: ms) = m :: toList ms

  public export
  reverse : ModuleSigsList -> ModuleSigsList
  reverse msl = fromList $ reverse $ toList msl

public export
allInputs : {ms : ModuleSigsList} -> FinsList ms.length -> SVObjList
allInputs []      = []
allInputs (i::is) = (index ms i).inputs ++ allInputs is

public export
allOutputs : {ms : ModuleSigsList} -> FinsList ms.length -> SVObjList
allOutputs []      = []
allOutputs (i::is) = (index ms i).outputs ++ allOutputs is

public export
totalInputs : {ms : ModuleSigsList} -> FinsList ms.length -> Nat
totalInputs = length . allInputs

public export
totalOutputs : {ms : ModuleSigsList} -> FinsList ms.length -> Nat
totalOutputs = length . allOutputs

public export
basicIntegral : SVType -> Bool
basicIntegral (RVar x)          = False
basicIntegral (AVar x)          = True
basicIntegral (VVar x)          = True
basicIntegral (PackedArr   t _) = True
basicIntegral (UnpackedArr x _) = basicIntegral x

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
  UU : EqSuperBasic t t' ->
       EqSuperBasic (UnpackedArr t {p} shape) (UnpackedArr t' {p = p'} shape')

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
  EqUArr : EqUnpackedArrSig t t' -> So (shape `isDimensionCompatibleWith` shape') ->
    EqUnpackedArrSig (UnpackedArr t {p} shape) (UnpackedArr t' {p = p'} shape')

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

  public export
  lookUp : (fl : FinsList n) -> Fin (fl.length) -> Fin n
  lookUp = index

  public export
  listLookUp : (y : FinsList n) -> FinsList (y.length) -> FinsList n
  listLookUp xs []        = []
  listLookUp xs (y :: ys) = lookUp xs y :: listLookUp xs ys


||| 10.3.2 The continuous assignment statement
||| Nets can be driven by multiple continuous assignments or by a mixture of primitive outputs, module outputs,
||| and continuous assignments.
||| IEEE 1800-2023
public export
data Multidriven : SVObject -> Type where
  RN : ResolvedNet sv => Multidriven sv

public export
isMD : SVObject -> Bool
isMD (Net Supply0' t) = True
isMD (Net Supply1' t) = True
isMD (Net Triand'  t) = True
isMD (Net Trior'   t) = True
isMD (Net Trireg'  t) = True
isMD (Net Tri0'    t) = True
isMD (Net Tri1'    t) = True
isMD (Net Tri'     t) = True
isMD (Net Wire'    t) = True
isMD (Net Wand'    t) = True
isMD (Net Wor'     t) = True
isMD _                = False

||| 10.3.2 The continuous assignment statement
||| Variables can only be driven by one continuous assignment or by one primitive output or module output.
||| IEEE 1800-2023
public export
data SingleDriven : SVObject -> Type where
  SDV : SingleDriven (Var st)
  SDU : AllowedNetData st => SingleDriven (Net Uwire' st)

public export
isSD : SVObject -> Bool
isSD (Var st)        = True
isSD (Net Uwire' st) = True
isSD _               = False

namespace MultiConnection
  ||| Unsafe, but simple and effective way to hold indexes of sinks and sources
  public export
  data MultiConnection : (ms : ModuleSigsList) -> (m : ModuleSig) -> (subMs : FinsList ms.length) -> Type where
    MkMC : {ms : _} -> {m : _} -> {subMs : _} ->
           (tsk : MFin $ topSnks' m) -> (ssk : FinsList $ subSnks' ms m subMs) ->
           (tsc : MFin $ topSrcs' m) -> (ssc : FinsList $ subSrcs' ms m subMs) -> MultiConnection ms m subMs

  ||| 6.10 Implicit declarations
  ||| If an identifier is used in the terminal list of a primitive instance or in the port connection list of a
  ||| module, interface, program, or static checker instance (but not a procedural checker instance, see
  ||| 17.3), and that identifier has not been declared previously in the scope where the instantiation
  ||| appears or in any scope whose declarations can be directly referenced from the scope where the
  ||| instantiation appears (see 23.9), then an implicit scalar net of default net type shall be assumed.
  public export
  unpOrDefault : (munp : SVObject) -> SVObject
  unpOrDefault munp = if isUnpacked munp then munp else defaultNetType

  public export
  Empty : {ms : _} -> {m : _} -> {subMs: _} -> MultiConnection ms m subMs
  Empty = MkMC Nothing [] Nothing []

  public export
  mOrElse : Maybe SVObject -> SVObject -> SVObject
  mOrElse Nothing  d = d
  mOrElse (Just x) _ = x

  public export
  mtype : SVObjList -> Maybe SVObject
  mtype []       = Nothing
  mtype (s :: _) = Just $ unpOrDefault s

  public export
  types : (sv : SVObjList) -> FinsList (sv.length) -> SVObjList
  types sv []        = []
  types sv (f :: fs) = typeOf sv f :: types sv fs

  public export
  findUnpNet : SVObjList -> Maybe SVObject
  findUnpNet []        = Nothing
  findUnpNet (s :: sv) = if isUnpacked s && isMD s then Just s else findUnpNet sv

  public export
  resolveConnType : {ms : _} -> {m : _} -> {subMs : _} ->
                    (ssk : FinsList $ subSnks' ms m subMs) -> (ssc : FinsList $ subSrcs' ms m subMs) -> SVObject
  resolveConnType ssk ssc = let allSubPorts = types (subSnks ms m subMs) ssk ++ types (subSrcs ms m subMs) ssc in
    mOrElse (findUnpNet allSubPorts) $ mOrElse (mtype allSubPorts) defaultNetType

  public export
  typeOf : MultiConnection ms m subMs -> SVObject
  typeOf (MkMC (Just f) _   _        _)   = typeOf (topSnks m) f
  typeOf (MkMC _        _   (Just f) _)   = typeOf (topSrcs m) f
  typeOf (MkMC _        ssk _        ssc) = resolveConnType {ms} {m} {subMs} ssk ssc

  public export
  data MMC : (ms : ModuleSigsList) -> (m : ModuleSig) -> (subMs : FinsList ms.length) -> Type where
    Nothing : MMC ms m subMs
    Just : MultiConnection ms m subMs -> MMC ms m subMs

  public export
  data JustMC : MMC ms m subMs -> MultiConnection ms m subMs -> Type where
    JMMMCP : JustMC (Just x) x

  public export
  data MultiConnectionsList : (ms : ModuleSigsList) -> (m : ModuleSig) -> (subMs : FinsList ms.length) -> Type where
    Nil  : MultiConnectionsList ms m subMs
    (::) : MultiConnection ms m subMs -> MultiConnectionsList ms m subMs -> MultiConnectionsList ms m subMs

  public export
  length : MultiConnectionsList ms m subMs -> Nat
  length []      = 0
  length (x::xs) = S $ length xs

  public export
  index : (fs : MultiConnectionsList ms m subMs) -> Fin (length fs) -> MultiConnection ms m subMs
  index (f::_ ) FZ     = f
  index (_::fs) (FS i) = index fs i

  public export
  toVect : (mcs : MultiConnectionsList ms m subMs) -> Vect (length mcs) $ MultiConnection ms m subMs
  toVect []      = []
  toVect (x::xs) = x :: toVect xs

  public export
  toList : MultiConnectionsList ms m subMs -> List $ MultiConnection ms m subMs
  toList []      = []
  toList (x::xs) = x :: toList xs

  public export
  replaceAt : (fs : MultiConnectionsList ms m subMs) -> Fin (length fs) -> MultiConnection ms m subMs -> MultiConnectionsList ms m subMs
  replaceAt (_::xs) FZ     y  = y :: xs
  replaceAt (x::xs) (FS k) y  = x :: replaceAt xs k y


public export
data FillMode : (ms : ModuleSigsList) -> (m : ModuleSig) -> (subMs : FinsList ms.length) -> Nat -> Type where
  TSK : FillMode ms m subMs $ topSnks' m
  SSK : FillMode ms m subMs $ subSnks' ms m subMs
  TSC : FillMode ms m subMs $ topSrcs' m
  SSC : FillMode ms m subMs $ subSrcs' ms m subMs

public export
superReplaceTK : Fin (topSnks' m) -> MultiConnection ms m subMs -> MMC ms m subMs
superReplaceTK f (MkMC Nothing ssc Nothing ssk) = Just $ MkMC (Just f) ssc Nothing ssk
superReplaceTK f _                              = Nothing

public export
superReplaceSK : Fin (subSnks' ms m subMs) -> MultiConnection ms m subMs -> MMC ms m subMs
superReplaceSK f (MkMC tsk ssk tsc ssc) = Just $ MkMC tsk (f::ssk) tsc ssc

public export
superReplaceTC : Fin (topSrcs' m) -> MultiConnection ms m subMs -> MMC ms m subMs
superReplaceTC f (MkMC Nothing ssc Nothing ssk) = Just $ MkMC Nothing ssc (Just f) ssk
superReplaceTC f _                              = Nothing

public export
superReplaceSC : Fin (subSrcs' ms m subMs) -> MultiConnection ms m subMs -> MMC ms m subMs
superReplaceSC f (MkMC tsk ssk tsc ssc) = Just $ MkMC tsk ssk tsc (f::ssc)

public export
ultraSuperReplace : FillMode ms m subMs n -> Fin n -> MultiConnection ms m subMs -> MMC ms m subMs
ultraSuperReplace TSK = superReplaceTK
ultraSuperReplace SSK = superReplaceSK
ultraSuperReplace TSC = superReplaceTC
ultraSuperReplace SSC = superReplaceSC

public export
typeOfPort : (ms : ModuleSigsList) -> (m : ModuleSig) -> (subMs : FinsList ms.length) -> FillMode ms m subMs n -> Fin n ->  SVObject
typeOfPort ms m subMs TSK = typeOf (topSnks m)
typeOfPort ms m subMs SSK = typeOf (subSnks ms m subMs)
typeOfPort ms m subMs TSC = typeOf (topSrcs m)
typeOfPort ms m subMs SSC = typeOf (subSrcs ms m subMs)

public export
noSource : MultiConnection ms m subMs -> Bool
noSource (MkMC tsk ssk Nothing []) = True
noSource _                         = False

public export
data CanAddPort : FillMode ms m subMs n -> MultiConnection ms m subMs -> Type where
  YTK   : CanAddPort TSK mc
  YSK   : CanAddPort SSK mc
  YTCMD : Multidriven (typeOf mc) => CanAddPort TSC mc
  YSCMD : Multidriven (typeOf mc) => CanAddPort SSC mc
  YTCSD : So (noSource mc) => CanAddPort TSC mc
  YSCSD : So (noSource mc) => CanAddPort SSC mc

public export
data FitAny : {ms : ModuleSigsList} -> {m : ModuleSig} -> {subMs : FinsList ms.length} -> {n : _} ->
              MultiConnectionsList ms m subMs -> (i : Fin n) -> FillMode ms m subMs n -> MultiConnectionsList ms m subMs -> Type where
  NewAny      : (jmc : JustMC (ultraSuperReplace {ms} {m} {subMs} mode i Empty) newMC) -> FitAny {ms} {m} {subMs} rest i mode $ newMC :: rest
  ExistingAny : (f : Fin $ length rest) ->
                (cap : CanAddPort {ms} {m} {subMs} mode $ index rest f) ->
                (jmc : JustMC (ultraSuperReplace {ms} {m} {subMs} mode i $ index rest f) newMC) ->
                (cc : CanConnect (valueOf $ typeOf $ index rest f) (valueOf $ typeOfPort ms m subMs mode i)) ->
                FitAny {ms} {m} {subMs} rest i mode $ replaceAt rest f newMC

public export
natToFin' : Nat -> (n : Nat) -> MFin n
natToFin' i n = case natToFin i n of
  Nothing  => Nothing
  (Just x) => Just x

public export
data JustFin : MFin n -> Fin n -> Type where
  JF : JustFin (Just x) x

-- i has type Nat instead of Fin, because otherwise a fuel-consuming generator is derived
-- feature request : https://github.com/buzden/deptycheck/issues/279
public export
data FillAny : {ms : ModuleSigsList} -> {m : ModuleSig} -> {subMs : FinsList ms.length} ->
               (pre : MultiConnectionsList ms m subMs) -> {n : _} -> (i : Nat) ->
               FillMode ms m subMs n -> (aft : MultiConnectionsList ms m subMs) -> Type where
  FANil  : FillAny pre Z mode pre
  FACons : {jf : JustFin (natToFin' i n) f} -> (fit : FitAny {ms} {m} {subMs} {n} mid f mode aft) ->
           (rest : FillAny {ms} {m} {subMs} pre {n} i mode mid) ->
           FillAny {ms} {m} {subMs} pre {n} (S i) mode aft

public export
data GenMulticonns : (ms : ModuleSigsList) -> (m : ModuleSig) -> (subMs : FinsList ms.length) -> MultiConnectionsList ms m subMs -> Type where
  MkG : (ftk : FillAny {ms} {m} {subMs} []     (topSnks' m)          TSK fillTK) ->
        (fsk : FillAny {ms} {m} {subMs} fillTK (subSnks' ms m subMs) SSK fillSK) ->
        (ftc : FillAny {ms} {m} {subMs} fillSK (topSrcs' m)          TSC fillTC) ->
        (fsc : FillAny {ms} {m} {subMs} fillTC (subSrcs' ms m subMs) SSC fillSC) ->
        GenMulticonns ms m subMs fillSC

public export
data Modules : ModuleSigsList -> Type where

  End : Modules ms

  NewCompositeModule :
    (m : ModuleSig) ->
    (subMs : FinsList ms.length) ->
    {mcs : _} ->
    (0 _ : GenMulticonns ms m subMs mcs) ->
    (cont : Modules $ m::ms) ->
    Modules ms


export
genFitAny : Fuel -> {ms : ModuleSigsList} -> {m : ModuleSig} -> {subMs : FinsList ms.length} -> {n : _} ->
            (rest : MultiConnectionsList ms m subMs) -> (i : Fin n) -> (mode : FillMode ms m subMs n) ->
            Gen MaybeEmpty (aft : MultiConnectionsList ms m subMs ** FitAny {ms} {m} {subMs} rest i mode aft)

export
genJF : Fuel -> {n : _} -> (mf : MFin n) -> Gen MaybeEmpty (f : Fin n ** JustFin mf f)

export
genFillAny : Fuel -> {ms : ModuleSigsList} -> {m : ModuleSig} -> {subMs : FinsList ms.length} ->
                     (pre : MultiConnectionsList ms m subMs) -> {n : _} -> (i : Nat) -> (mode : FillMode ms m subMs n) ->
                     Gen MaybeEmpty (aft : MultiConnectionsList ms m subMs ** FillAny {ms} {m} {subMs} {n} pre i mode aft)
genFillAny x pre Z     mode = pure (pre ** FANil)
genFillAny x pre (S i) mode = do
  (mid ** rest) <- genFillAny {ms} {m} {subMs} x pre i mode
  (f ** jf) <- genJF x $ natToFin' i n
  (aft ** fit) <- genFitAny {ms} {m} {subMs} x mid f mode
  pure (aft ** FACons {jf} fit rest)

export
genModules : Fuel -> (ms : ModuleSigsList) ->
  (Fuel -> {ms' : ModuleSigsList} -> {m' : ModuleSig} -> {subMs' : FinsList ms'.length} ->
  (pre' : MultiConnectionsList ms' m' subMs') -> {n' : _} -> (i' : Nat) -> (mode' : FillMode ms' m' subMs' n') ->
  Gen MaybeEmpty (aft' : MultiConnectionsList ms' m' subMs' ** FillAny {ms=ms'} {m=m'} {subMs=subMs'} {n=n'} pre' i' mode' aft')) =>
  Gen MaybeEmpty $ Modules ms
