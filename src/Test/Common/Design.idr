module Test.Common.Design

import Data.Fin
import Data.Fuel
import Data.Vect

import Test.DepTyCheck.Gen

import public Test.Common.Utils
import public Test.Common.DataType

%default total

namespace DesignUnitSig

  public export
  record DesignUnitSig l where
    constructor MkDesignUnitSig
    inputs  : DataTypesList l
    outputs : DataTypesList l

  public export
  (.inpsCount) : DesignUnitSig l -> Nat
  (.inpsCount) m = m.inputs.length

  public export
  (.outsCount) : DesignUnitSig l -> Nat
  (.outsCount) m = m.outputs.length

  %name DesignUnitSig d

  public export
  data DesignUnitSigsList : Lang -> Type where
    Nil  : DesignUnitSigsList l
    (::) : DesignUnitSig l -> DesignUnitSigsList l -> DesignUnitSigsList l

  %name DesignUnitSigsList usl

  public export
  length : DesignUnitSigsList l -> Nat
  length []      = Z
  length (_::usl) = S $ length usl

  public export %inline
  (.length) : DesignUnitSigsList l -> Nat
  (.length) = length

  public export
  index : (usl : DesignUnitSigsList l) -> Fin usl.length -> DesignUnitSig l
  index (sig::_ ) FZ    = sig
  index (_::usl) (FS i) = index usl i

namespace MultiConnection

  public export
  totalTops : DesignUnitSig l -> DataTypesList l
  totalTops s = s.inputs ++ s.outputs

  public export
  totalTops' : DesignUnitSig l -> Nat
  totalTops' s = (totalTops s).length

  public export
  totalSubs : (usl : DesignUnitSigsList l) -> (subUs : FinsList usl.length) -> DataTypesList l
  totalSubs usl []      = []
  totalSubs usl (f::fs) = (totalTops $ index usl f) ++ totalSubs usl fs

  public export
  totalSubs' : (usl : DesignUnitSigsList l) -> (subUs : FinsList usl.length) -> Nat
  totalSubs' usl subUs = length $ totalSubs usl subUs

  public export
  typeOfSubPort : (usl : DesignUnitSigsList l) -> (subUs : FinsList usl.length) -> Fin (totalSubs' usl subUs) -> DataType l
  typeOfSubPort usl subUs = index (totalSubs usl subUs)

  public export
  data MCNotEmpty : MFin a -> FinsList b -> Type where
    JustTop : MCNotEmpty (Just x) sub
    JustSub : MCNotEmpty top      (x :: xs)

  public export
  data MultiConnection : (l : Lang) -> (s : DesignUnitSig l) ->
                         (usl : DesignUnitSigsList l) -> (subUs : FinsList usl.length) -> Type where
    MkMC : {l : _} -> {s : _} -> {usl : _} -> {subUs : _} ->
           (top : MFin $ totalTops' s) -> (subs : FinsList $ totalSubs' usl subUs) -> (ne : MCNotEmpty top subs) => MultiConnection l s usl subUs
  
  public export
  data MultiConnectionsList : (l : Lang) -> (s : DesignUnitSig l) ->
                              (usl : DesignUnitSigsList l) -> (subUs : FinsList usl.length) -> Type where
    Nil  : MultiConnectionsList l s usl subUs
    (::) : MultiConnection l s usl subUs -> MultiConnectionsList l s usl subUs -> MultiConnectionsList l s usl subUs
  
  public export
  length : MultiConnectionsList l s usl subUs -> Nat
  length []       = Z
  length (_::mcs) = S $ length mcs

  public export
  index : (mcs : MultiConnectionsList l s usl subUs) -> Fin (length mcs) -> MultiConnection l s usl subUs
  index (mc::_ ) FZ     = mc
  index (_::mcs) (FS i) = index mcs i

  public export
  toVect : (mcs : MultiConnectionsList l s usl subUs) -> Vect (length mcs) $ MultiConnection l s usl subUs
  toVect []         = []
  toVect (m :: mcs) = m :: toVect mcs

  public export
  typeOf : {l : _} -> {s : _} -> {usl : _} -> {subUs : _} ->
          MultiConnection l s usl subUs -> DataType l
  typeOf MkMC Nothing [] impossible
  typeOf MkMC Nothing [] impossible
  typeOf {l = SystemVerilog} (MkMC Nothing (f :: _)) = SVT $ 1 -- todo: change
  typeOf {l = SystemVerilog} (MkMC (Just f) _      ) = index (totalTops s) f
  typeOf {l = VHDL}          (MkMC Nothing (f :: _)) = index (totalSubs usl subUs) f
  typeOf {l = VHDL}          (MkMC (Just f) _      ) = index (totalTops s) f

  ||| Find type of port by fin 
  public export
  findTypeTop : {l : _} -> {s : _} -> {usl : _} -> {subUs : _} ->
                Fin (totalTops' s) -> (mcs : MultiConnectionsList l s usl subUs) -> Maybe $ DataType l
  findTypeTop f []        = Nothing -- impossible
  findTypeTop f (   (MkMC Nothing  subs) :: xs) = findTypeTop f xs
  findTypeTop f (mc@(MkMC (Just x) subs) :: xs) = case f == x of
    False => findTypeTop f xs
    True  => Just $ typeOf mc

  public export
  topiToTotal : {s : _} -> Fin (s.inpsCount) -> Fin (totalTops' s)
  topiToTotal {s} f = fixDTLFin $ weakenN s.outsCount f

  public export
  topoToTotal : {s : _} -> Fin (s.outsCount) -> Fin (totalTops' s)
  topoToTotal {s} f = fixDTLFin $ shift s.inpsCount f

  public export
  findTypeTI : {l : _} -> {s : _} -> {usl : _} -> {subUs : _} ->
              Fin (s.inpsCount) -> (mcs : MultiConnectionsList l s usl subUs) -> Maybe $ DataType l
  findTypeTI f = findTypeTop $ topiToTotal f

  public export
  findTypeTO : {l : _} -> {s : _} -> {usl : _} -> {subUs : _} ->
              Fin (s.outsCount) -> (mcs : MultiConnectionsList l s usl subUs) -> Maybe $ DataType l
  findTypeTO f = findTypeTop $ topoToTotal f

namespace CAP

  public export
  isResolved : DataType l -> Bool
  isResolved (SVT _)        = True -- TODO: make uwire net unresolved
  isResolved (VHD StdLogic) = True
  isResolved (VHD _)        = False

  isTopSink : {s : _} -> MFin (totalTops' s) -> Bool
  isTopSink Nothing  = False
  isTopSink (Just f) = finToNat f < s.inpsCount

  subsOnlySinks : (subs : FinsList $ totalSubs' usl subUs) -> Bool
  subsOnlySinks []        = True
  subsOnlySinks (f :: fs) = False || subsOnlySinks fs

  public export
  noSource : {s : _} -> {usl : _} -> {subUs : _} -> MultiConnection l s usl subUs -> Bool
  noSource (MkMC top subs) = isTopSink top && subsOnlySinks subs

  public export
  data CanAddSubPort : MultiConnection l s usl subUs -> Type where
    NoSource : CanAddSubPort mc
    IsMultidriven : So (isResolved $ typeOf mc) => CanAddSubPort mc
    -- no source or multidriven

namespace Connection

  public export
  data SamePredefinedEnumeration : PredefinedEnumeration -> PredefinedEnumeration -> Type where
    SCC : SamePredefinedEnumeration CHARACTER CHARACTER
    SBB : SamePredefinedEnumeration BIT BIT
    SBO : SamePredefinedEnumeration BOOLEAN BOOLEAN
    SSS : SamePredefinedEnumeration SEVERITY_LEVEL SEVERITY_LEVEL

  public export
  data CanConnectVHDL : DataType VHDL -> DataType VHDL -> Type where
    CCInt : CanConnectVHDL (VHD $ Integer') (VHD $ Integer')
    CCPhy : CanConnectVHDL (VHD $ Physical) (VHD $ Physical)
    CCRea : CanConnectVHDL (VHD $ Real) (VHD $ Real)
    CCEn  : SamePredefinedEnumeration e e' -> CanConnectVHDL (VHD $ Enum e) (VHD $ Enum e')

  public export
  data CanConnect : (l : Lang) -> DataType l -> DataType l -> Type where
    CCSV : CanConnect SystemVerilog t t'
    CCVH : CanConnectVHDL t t' -> CanConnect VHDL t t'

namespace GenMulticonns

  public export
  natToFin' : Nat -> (n : Nat) -> MFin n
  natToFin' i n = case natToFin i n of
    Nothing  => Nothing
    (Just x) => Just x

  public export
  data JustFin : MFin n -> Fin n -> Type where
    JF : JustFin (Just x) x

  public export
  newTop : {l : _} -> {s : _} -> {usl : _} -> {subUs : _} ->
           Fin (totalTops' s) -> MultiConnection l s usl subUs
  newTop f = MkMC (Just f) []

  public export
  newSub : {l : _} -> {s : _} -> {usl : _} -> {subUs : _} ->
           Fin (totalSubs' usl subUs) -> MultiConnection l s usl subUs
  newSub f = MkMC Nothing [ f ]

  public export
  addSub : Fin (totalSubs' usl subUs) -> MultiConnection l s usl subUs ->  MultiConnection l s usl subUs
  addSub f (MkMC top subs) = MkMC top (f :: subs)

  public export
  insertAt0 : (pre : MultiConnectionsList l s usl subUs) -> MultiConnection l s usl subUs -> MultiConnectionsList l s usl subUs
  insertAt0 pre mc = mc :: pre

  public export
  replaceAt : (pre : MultiConnectionsList l s usl subUs) -> Fin (length pre) -> MultiConnection l s usl subUs -> MultiConnectionsList l s usl subUs
  replaceAt []        FZ     _ impossible
  replaceAt []        (FS _) _ impossible
  replaceAt (_ :: xs) FZ     mc = mc :: xs
  replaceAt (x :: xs) (FS i) mc = x :: replaceAt xs i mc

  public export
  data FillTop : (l : Lang) -> (s : DesignUnitSig l) ->
                 (usl : DesignUnitSigsList l) -> (subUs : FinsList usl.length) ->
                 MultiConnectionsList l s usl subUs -> (topi : Nat) -> MultiConnectionsList l s usl subUs -> Type where
    TEnd      : FillTop l s usl subUs pre 0 pre
    TNew      : (recur : FillTop l s usl subUs pre k mid) ->
                {jf : JustFin (natToFin' k $ totalTops' s) topF} ->
                FillTop l s usl subUs pre (S k) $ insertAt0 mid   $ newTop topF
  
  public export
  data FillSub : (l : Lang) -> (s : DesignUnitSig l) ->
                 (usl : DesignUnitSigsList l) -> (subUs : FinsList usl.length) ->
                 MultiConnectionsList l s usl subUs -> (subi : Nat) -> MultiConnectionsList l s usl subUs -> Type where
    SEnd      : FillSub l s usl subUs pre 0 pre
    SNew      : (recur : FillSub l s usl subUs pre k mid) ->
                {jf : JustFin (natToFin' k $ totalSubs' usl subUs) subF} -> 
                FillSub l s usl subUs pre (S k) $ insertAt0 mid   $ newSub subF
    SExisting : (recur : FillSub l s usl subUs pre k mid) ->
                (f : Fin $ length mid) -> {jf : JustFin (natToFin' k $ totalSubs' usl subUs) subF} ->
                (cc : CanConnect l (typeOf $ index mid f)  (typeOfSubPort usl subUs subF)) ->
                FillSub l s usl subUs pre (S k) $ replaceAt mid f $ addSub subF $ index mid f
                -- `index mid f` is where we add port
                -- subF is which port we add

  public export
  data GenMulticonns : (l : Lang) -> (s : DesignUnitSig l) ->
                       (usl : DesignUnitSigsList l) -> (subUs : FinsList usl.length) -> 
                       MultiConnectionsList l s usl subUs -> Type where
    GenMC : (ft : FillTop l s usl subUs []        (totalTops' s)         filledTop) -> 
            (fs : FillSub l s usl subUs filledTop (totalSubs' usl subUs) filledSub) ->
            GenMulticonns l s usl subUs filledSub

namespace DesignUnit

  public export
  data DesignUnit : {l : _} -> 
                    (s : DesignUnitSig l) -> 
                    (usl : DesignUnitSigsList l) -> 
                    (subUs : FinsList usl.length) ->
                    (mcs : MultiConnectionsList l s usl subUs) -> Type where
    MkDesign : (s : DesignUnitSig l) ->
               {usl : _} ->
               (subUs : FinsList usl.length) ->
               (mcs : MultiConnectionsList l s usl subUs) ->
               {0 _ : GenMulticonns l s usl subUs mcs} ->
               DesignUnit {l} s usl subUs mcs

  public export
  data DesignUnitsList : DesignUnitSigsList l -> Type where
    Nil  : DesignUnitsList usl
    (::) : {s : _} -> {usl : _} -> {subUs : _} -> {mcs : _} -> 
           DesignUnit {l} s usl subUs mcs -> DesignUnitsList {l} (s::usl) -> DesignUnitsList {l} usl

export
genDesignUnitsList : Fuel -> (l : Lang) -> (usl : DesignUnitSigsList l) ->
  Gen MaybeEmpty $ DesignUnitsList usl
