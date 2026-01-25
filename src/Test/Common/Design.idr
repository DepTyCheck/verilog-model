module Test.Common.Design

import Data.Fin

import Test.Common.Utils

import Test.Verilog.SVType
import Test.VHDL.VHDLType

%default total

public export
data Lang = SystemVerilog | VHDL

namespace CommonDataType

  public export
  data DataType : Lang -> Type where
    SVT : SVType   -> DataType SystemVerilog
    VHT : VHDLType -> DataType VHDL
  
  public export
  data DataTypesList : Lang -> Type where
    Nil  : DataTypesList n
    (::) : DataType n -> DataTypesList n -> DataTypesList n

  %name DataTypesList ds

  public export
  (.asList) : DataTypesList n -> List $ DataType n
  (.asList) []      = []
  (.asList) (x::xs) = x :: xs.asList

  public export
  (.length) : DataTypesList n -> Nat
  (.length) []      = 0
  (.length) (x::xs) = S xs.length

  public export
  index : (fs : DataTypesList s) -> Fin fs.length -> DataType s
  index (f::_ ) FZ     = f
  index (_::fs) (FS i) = index fs i

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

  %name DesignUnitSig m

  public export
  data DesignUnitSigsList = Nil | (::) DesignUnitSig DesignUnitSigsList

  %name DesignUnitSigsList ms

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

  public export
  (++) : DesignUnitSigsList -> DesignUnitSigsList -> DesignUnitSigsList
  Nil       ++ ys = ys
  (x :: xs) ++ ys = x :: (xs ++ ys)

namespace DesignUnit
  public export
  data DesignUnits : DesignUnitSigsList -> Type

namespace MultiConnection
  public export
  data MultiConnectionsList : DesignUnits dus -> Type

namespace GenMulticonns
  public export
  data GenMulticonns : (du : DesignUnits dus) -> MultiConnectionsList du -> Type

namespace DesignUnit

  public export
  data DesignUnits : DesignUnitSigsList -> Type where
    End : DesignUnits us
    New :
      (m : DesignUnitSig) ->
      (subMs : FinsList ms.length) ->
      -- {mcs : _} ->
      -- (0 _ : GenMulticonns ms m subMs mcs) ->
      (cont : DesignUnits $ m::ms) ->
      DesignUnits ms

namespace MultiConnection

  public export
  tops : DesignUnits dus -> Nat

  public export
  subs : DesignUnits dus -> Nat

  -- data MultiConnection : DesignUnits dus -> Type
  -- data MCNotEmp

  public export
  data MultiConnection : DesignUnits dus -> Type where
    MkMC : (MFin $ tops du) -> (subs : FinsList $ subs du) -> MultiConnection du
  
  public export
  data MultiConnectionsList : DesignUnits dus -> Type where
    Nil  : MultiConnectionsList du
    (::) : MultiConnection du -> MultiConnectionsList du -> MultiConnectionsList du
  
  public export
  length : MultiConnectionsList du -> Nat
  length []      = Z
  length (_::ms) = S $ length ms

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
  newTop : Fin (tops du) -> MultiConnection du
  newTop f = MkMC (Just f) []

  public export
  newSub : Fin (subs du) -> MultiConnection du
  newSub f = MkMC Nothing [ f ]

  public export
  insertAt0 : (pre : MultiConnectionsList du) -> MultiConnection du -> MultiConnectionsList du
  insertAt0 pre mc = mc :: pre

  public export
  insertAt : (pre : MultiConnectionsList du) -> Fin (length pre) -> MultiConnection du -> MultiConnectionsList du
  insertAt     []        FZ     _ impossible
  insertAt     []        (FS _) _ impossible
  insertAt pre@(_ :: _ ) FZ     mc = mc :: pre
  insertAt     (x :: xs) (FS i) mc = insertAt xs i mc

  public export
  data FillTop : (du : DesignUnits dus) -> MultiConnectionsList du -> (topi : Nat) -> MultiConnectionsList du -> Type where
    End      : FillTop du pre 0 pre
    New      :                           {jf : JustFin (natToFin' k $ tops du) topF} -> FillTop du pre (S k) $ insertAt0 pre   $ newTop topF
    Existing : (f : Fin $ length pre) -> {jf : JustFin (natToFin' k $ tops du) topF} -> FillTop du pre (S k) $ insertAt  pre f $ newTop topF

  public export
  data GenMulticonns : (du : DesignUnits dus) -> MultiConnectionsList du -> Type where
    TODO : GenMulticonns du mcl
  -- where
  --   MkMC : (MFin $ tops du) -> (subs : FinsList $ subs du) -> MultiConnection du

-- public export
-- data MultiConnsCtx : DesignUnits -> Type where
--   NoConns :

  -- public export
  -- fromList : List DesignUnitSig -> DesignUnitSigsList
  -- fromList [] = []
  -- fromList (x :: xs) = x :: fromList xs

  -- public export
  -- toList : DesignUnitSigsList -> List DesignUnitSig
  -- toList []        = []
  -- toList (m :: ms) = m :: toList ms

  -- public export
  -- reverse : DesignUnitSigsList -> DesignUnitSigsList
  -- reverse msl = fromList $ reverse $ toList msl
