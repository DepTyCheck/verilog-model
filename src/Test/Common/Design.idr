module Test.Common.Design

import Data.Fin
import Data.Fuel

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

  %name DesignUnitSigsList ds

  public export
  length : DesignUnitSigsList l -> Nat
  length []      = Z
  length (_::ms) = S $ length ms

  public export %inline
  (.length) : DesignUnitSigsList l -> Nat
  (.length) = length

  public export
  index : (ms : DesignUnitSigsList l) -> Fin ms.length -> DesignUnitSig l
  index (m::_ ) FZ     = m
  index (_::ms) (FS i) = index ms i

namespace DesignUnit

  public export
  data DesignUnits : DesignUnitSigsList l -> Type

namespace MultiConnection

  public export
  totalTops : DesignUnitSig l -> Nat
  totalTops du = du.inpsCount + du.outsCount

  public export
  totalSubs : (uu : DesignUnitSigsList l) -> (subUs : FinsList uu.length) -> Nat
  totalSubs uu []      = 0
  totalSubs uu (f::fs) = (totalTops $ index uu f) + totalSubs uu fs

  public export
  data MultiConnection : (l : Lang) -> (uu : DesignUnitSigsList l) -> 
                         (u : DesignUnitSig l) -> (subUs : FinsList uu.length) -> Type where
    MkMC : (MFin $ totalTops u) -> (subs : FinsList $ totalSubs uu subUs) -> MultiConnection l uu u subUs
  
  public export
  data MultiConnectionsList : (l : Lang) -> (uu : DesignUnitSigsList l) -> 
                              (u : DesignUnitSig l) -> (subUs : FinsList uu.length) -> Type where
    Nil  : MultiConnectionsList l uu u subUs
    (::) : MultiConnection l uu u subUs -> MultiConnectionsList l uu u subUs -> MultiConnectionsList l uu u subUs
  
  public export
  length : MultiConnectionsList l uu u subUs -> Nat
  length []      = Z
  length (_::ms) = S $ length ms

  public export
  index : (ms : MultiConnectionsList l uu u subUs) -> Fin (length ms) -> MultiConnection l uu u subUs
  index (m::_ ) FZ     = m
  index (_::ms) (FS i) = index ms i

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
  newTop : Fin (totalTops u) -> MultiConnection l uu u subUs
  newTop f = MkMC (Just f) []

  public export
  newSub : Fin (totalSubs uu subUs) -> MultiConnection l uu u subUs
  newSub f = MkMC Nothing [ f ]

  public export
  addSub : Fin (totalSubs uu subUs) -> MultiConnection l uu u subUs ->  MultiConnection l uu u subUs
  addSub f (MkMC top subs) = MkMC top $ f :: subs

  public export
  insertAt0 : (pre : MultiConnectionsList l uu u subUs) -> MultiConnection l uu u subUs -> MultiConnectionsList l uu u subUs
  insertAt0 pre mc = mc :: pre

  public export
  replaceAt : (pre : MultiConnectionsList l uu u subUs) -> Fin (length pre) -> MultiConnection l uu u subUs -> MultiConnectionsList l uu u subUs
  replaceAt []        FZ     _ impossible
  replaceAt []        (FS _) _ impossible
  replaceAt (_ :: xs) FZ     mc = mc :: xs
  replaceAt (x :: xs) (FS i) mc = replaceAt xs i mc

  public export
  data FillTop : (l : Lang) ->(uu : DesignUnitSigsList l) -> 
                 (u : DesignUnitSig l) -> (subUs : FinsList uu.length) ->
                 MultiConnectionsList l uu u subUs -> (topi : Nat) -> MultiConnectionsList l uu u subUs -> Type where
    TEnd      : FillTop l uu u subUs pre 0 pre
    TNew      : {jf : JustFin (natToFin' k $ totalTops u) topF} -> 
                FillTop l uu u subUs pre (S k) $ insertAt0 pre   $ newTop topF
  
  public export
  data FillSub : (l : Lang) ->(uu : DesignUnitSigsList l) -> 
                 (u : DesignUnitSig l) -> (subUs : FinsList uu.length) ->
                 MultiConnectionsList l uu u subUs -> (subi : Nat) -> MultiConnectionsList l uu u subUs -> Type where
    SEnd      : FillSub l uu u subUs pre 0 pre
    SNew      :                           {jf : JustFin (natToFin' k $ totalSubs uu subUs) subF} -> 
                FillSub l uu u subUs pre (S k) $ insertAt0 pre   $ newSub subF
    SExisting : (f : Fin $ length pre) -> {jf : JustFin (natToFin' k $ totalSubs uu subUs) subF} -> 
                FillSub l uu u subUs pre (S k) $ replaceAt pre f $ addSub subF $ index pre f

  public export
  data GenMulticonns : (l : Lang) ->(uu : DesignUnitSigsList l) -> 
                       (u : DesignUnitSig l) -> (subUs : FinsList uu.length) -> 
                       MultiConnectionsList l uu u subUs -> Type where
    GenMC : (ft : FillTop l uu u subUs []        (totalTops u)        filledTop) -> 
            (fs : FillSub l uu u subUs filledTop (totalSubs uu subUs) filledSub) ->
            GenMulticonns l uu u subUs filledSub

namespace DesignUnit

  public export
  data DesignUnits : DesignUnitSigsList l -> Type where
    End : DesignUnits uu
    New :
      (u : DesignUnitSig l) ->
      {uu : _} ->
      (subUs : FinsList uu.length) ->
      (mcs : MultiConnectionsList l uu u subUs) ->
      {0 _ : GenMulticonns l uu u subUs mcs} ->
      (cont : DesignUnits {l} $ u::uu) ->
      DesignUnits {l} uu

export
genDesignUnits : Fuel -> (l : Lang) -> (dus : DesignUnitSigsList l) ->
  Gen MaybeEmpty $ DesignUnits dus
