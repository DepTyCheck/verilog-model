module Test.Common.Design

import Data.Fin

import Test.Common.Utils

import Test.Common.DataType

import Test.VHDL.Mutation

%default total

namespace PredefinedUnit

  public export
  record PredefinedUnitSig lang where
    constructor MkPredefinedUnitSig
    inputs  : DataTypesList lang
    outputs : DataTypesList lang

  public export
  (.inpsCount) : PredefinedUnitSig l -> Nat
  (.inpsCount) m = m.inputs.length

  public export
  (.outsCount) : PredefinedUnitSig l -> Nat
  (.outsCount) m = m.outputs.length

  public export
  data PredefinedUnitSigsList : Lang -> Type where
    Nil  : PredefinedUnitSigsList l
    (::) : PredefinedUnitSig l -> PredefinedUnitSigsList l -> PredefinedUnitSigsList l

  %name PredefinedUnitSigsList ps

namespace DesignUnitSig

  public export
  record DesignUnitSig where
    constructor MkDesignUnitSig
    inputs  : Nat
    outputs : Nat

  -- public export
  -- (.inpsCount) : DesignUnitSig -> Nat
  -- (.inpsCount) m = length m.inputs

  -- public export
  -- (.outsCount) : DesignUnitSig -> Nat
  -- (.outsCount) m = length m.outputs

  %name DesignUnitSig d

  public export
  data DesignUnitSigsList = Nil | (::) DesignUnitSig DesignUnitSigsList

  %name DesignUnitSigsList ds

  public export
  length : DesignUnitSigsList -> Nat
  length []      = Z
  length (_::ms) = S $ length ms

  public export %inline
  (.length) : DesignUnitSigsList -> Nat
  (.length) = length

  public export
  index : (ms : DesignUnitSigsList) -> Fin ms.length -> DesignUnitSig
  index (m::_ ) FZ     = m
  index (_::ms) (FS i) = index ms i

  -- public export
  -- (++) : DesignUnitSigsList -> DesignUnitSigsList -> DesignUnitSigsList
  -- Nil       ++ ys = ys
  -- (x :: xs) ++ ys = x :: (xs ++ ys)

public export
totalLen : PredefinedUnitSigsList l -> DesignUnitSigsList -> Nat

namespace DesignUnit
  public export
  data DesignUnits : PredefinedUnitSigsList l -> DesignUnitSigsList -> Type

namespace MultiConnection

  public export
  totalTops : DesignUnitSig -> Nat

  public export
  totalSubs : (pu : PredefinedUnitSigsList l) -> (uu : DesignUnitSigsList) -> (subUs : FinsList $ totalLen pu uu) -> Nat

  -- data MultiConnection : DesignUnits dus -> Type
  -- data MCNotEmp

  public export
  data MultiConnection : (l : Lang) -> (pu : PredefinedUnitSigsList l) -> (uu : DesignUnitSigsList) -> 
                         (u : DesignUnitSig) -> (subUs : FinsList $ totalLen pu uu) -> Type where
    MkMC : (MFin $ totalTops u) -> (subs : FinsList $ totalSubs pu uu subUs) -> MultiConnection l pu uu u subUs
  
  public export
  data MultiConnectionsList : (l : Lang) -> (pu : PredefinedUnitSigsList l) -> (uu : DesignUnitSigsList) -> 
                              (u : DesignUnitSig) -> (subUs : FinsList $ totalLen pu uu) -> Type where
    Nil  : MultiConnectionsList l pu uu u subUs
    (::) : MultiConnection l pu uu u subUs -> MultiConnectionsList l pu uu u subUs -> MultiConnectionsList l pu uu u subUs
  
  public export
  length : MultiConnectionsList l pu uu u subUs -> Nat
  length []      = Z
  length (_::ms) = S $ length ms

  public export
  index : (ms : MultiConnectionsList l pu uu u subUs) -> Fin (length ms) -> MultiConnection l pu uu u subUs
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
  newTop : Fin (totalTops u) -> MultiConnection l pu uu u subUs
  newTop f = MkMC (Just f) []

  public export
  newSub : Fin (totalSubs pu uu subUs) -> MultiConnection l pu uu u subUs
  newSub f = MkMC Nothing [ f ]

  public export
  addSub : Fin (totalSubs pu uu subUs) -> MultiConnection l pu uu u subUs ->  MultiConnection l pu uu u subUs
  addSub f (MkMC top subs) = MkMC top $ f :: subs

  public export
  insertAt0 : (pre : MultiConnectionsList l pu uu u subUs) -> MultiConnection l pu uu u subUs -> MultiConnectionsList l pu uu u subUs
  insertAt0 pre mc = mc :: pre

  public export
  replaceAt : (pre : MultiConnectionsList l pu uu u subUs) -> Fin (length pre) -> MultiConnection l pu uu u subUs -> 
              MultiConnectionsList l pu uu u subUs
  replaceAt []        FZ     _ impossible
  replaceAt []        (FS _) _ impossible
  replaceAt (_ :: xs) FZ     mc = mc :: xs
  replaceAt (x :: xs) (FS i) mc = replaceAt xs i mc

  public export
  data FillTop : MultiConnectionsList l pu uu u subUs -> (topi : Nat) -> MultiConnectionsList l pu uu u subUs -> Type where
    TEnd      : FillTop pre 0 pre
    TNew      : {jf : JustFin (natToFin' k $ totalTops u) topF} -> 
                FillTop {pu} {uu} {u} {subUs} pre (S k) $ insertAt0 pre   $ newTop topF
    -- TExisting : (f : Fin $ length pre) -> {jf : JustFin (natToFin' k $ tops u) topF} -> 
    --            FillTop {pu} {uu} {u} {subUs} pre (S k) $ insertAt  pre f $ newTop topF
  
  public export
  data FillSub : MultiConnectionsList l pu uu u subUs -> (subi : Nat) -> MultiConnectionsList l pu uu u subUs -> Type where
    SEnd      : FillSub pre 0 pre
    SNew      :                           {jf : JustFin (natToFin' k $ totalSubs pu uu subUs) subF} -> 
                FillSub {pu} {uu} {u} {subUs} pre (S k) $ insertAt0 pre   $ newSub subF
    SExisting : (f : Fin $ length pre) -> {jf : JustFin (natToFin' k $ totalSubs pu uu subUs) subF} -> 
                FillSub {pu} {uu} {u} {subUs} pre (S k) $ replaceAt pre f $ addSub subF $ index pre f

  public export
  data GenMulticonns : (l : Lang) -> (pu : PredefinedUnitSigsList l) -> (uu : DesignUnitSigsList) -> 
                       (u : DesignUnitSig) -> (subUs : FinsList $ totalLen pu uu) -> 
                       MultiConnectionsList l pu uu u subUs -> Type where
    GenMC : FillTop {l} {pu} {uu} {u} {subUs} []        (totalTops u)           filledTop -> 
            FillSub {l} {pu} {uu} {u} {subUs} filledTop (totalSubs pu uu subUs) filledSub ->
            GenMulticonns l pu uu u subUs filledSub

namespace MultiConnsTypes

  ||| Returns type if multiconnection has a port which is connected to some external (and already defined) design unit
  public export
  hasType : (l : Lang) -> (pu : PredefinedUnitSigsList l) -> (uu : DesignUnitSigsList) -> 
            (subUs : FinsList $ totalLen pu uu) -> (subs : FinsList $ totalSubs pu uu subUs) -> Maybe $ DataType l
  
  public export
  data MDTisNothing : Maybe (DataType l) -> Type where
    YesNothing : MDTisNothing Nothing
  
  public export
  data MDTisJust: Maybe (DataType l) -> DataType l -> Type where
    YesJust : MDTisJust (Just t) t

  public export
  data ResolveDataType : (l : Lang) -> (pu : PredefinedUnitSigsList l) -> (uu : DesignUnitSigsList) -> 
                         (u : DesignUnitSig) -> (subUs : FinsList $ totalLen pu uu) -> MultiConnection l pu uu u subUs -> 
                         DataType l -> Type where
    Predef : MDTisJust    (hasType l pu uu subUs subs) t ->                     ResolveDataType l pu uu u subUs (MkMC top subs) t
    Newdef : MDTisNothing (hasType l pu uu subUs subs)   -> (t : DataType l) -> ResolveDataType l pu uu u subUs (MkMC top subs) t
  
  public export
  data Mutation : (l : Lang) -> (pre : DataType l) -> (res : DataType l) -> Type where
    MVHDL : MutationVHDL pre res -> Mutation VHDL pre res

  -- public export
  -- data MultiConnsTypes : MultiConnectionsList l pu uu u subUs -> Type where
  --   Nil  : MultiConnsTypes []
  --   (::) : (t : DataType l) -> {0 _ : ResolveDataType {pu} {uu} {u} {subUs} l c t} ->
  --          MultiConnsTypes {pu} {uu} {u} {subUs} l cs -> MultiConnsTypes {pu} {uu} {u} {subUs} l $ c::cs

namespace DesignUnit

  public export
  data DesignUnits : PredefinedUnitSigsList l -> DesignUnitSigsList -> Type where
    End : DesignUnits pu uu
    New :
      (u : DesignUnitSig) ->
      (subUs : FinsList $ totalLen pu uu) ->
      (mcs : MultiConnectionsList l pu uu u subUs) ->
      {0 _ : GenMulticonns l pu uu u subUs mcs} ->
      (cont : DesignUnits pu $ u::us) ->
      DesignUnits {l} pu us


-- namespace GenMulticonns


--   public export
--   data GenMulticonns : (du : DesignUnits dus) -> MultiConnectionsList du -> Type where
--     TODO : GenMulticonns du mcl
--   -- where
--   --   MkMC : (MFin $ tops du) -> (subs : FinsList $ subs du) -> MultiConnection du

-- -- public export
-- -- data MultiConnsCtx : DesignUnits -> Type where
-- --   NoConns :

--   -- public export
--   -- fromList : List DesignUnitSig -> DesignUnitSigsList
--   -- fromList [] = []
--   -- fromList (x :: xs) = x :: fromList xs

--   -- public export
--   -- toList : DesignUnitSigsList -> List DesignUnitSig
--   -- toList []        = []
--   -- toList (m :: ms) = m :: toList ms

--   -- public export
--   -- reverse : DesignUnitSigsList -> DesignUnitSigsList
--   -- reverse msl = fromList $ reverse $ toList msl
