module Test.Verilog

import Data.Fuel
import Data.Vect
import public Data.Fin

import Test.DepTyCheck.Gen

%default total

namespace Connections
  public export
  data ConnectionType = Logic | Wire | Uwire

  public export
  data ConnectionFeasibleRegion = D4 | Dint

  connFR : ConnectionType -> ConnectionFeasibleRegion
  connFR Logic = D4
  connFR Wire  = D4
  connFR Uwire = D4

  public export
  data ConnectionsList = Nil | (::) ConnectionType ConnectionsList

  public export
  length : ConnectionsList -> Nat
  length []      = Z
  length (_::ms) = S $ length ms

  public export %inline
  (.length) : ConnectionsList -> Nat
  (.length) = length

namespace ModuleSig

  public export
  record ModuleSig where
    constructor MkModuleSig
    inputs  : ConnectionsList
    outputs : ConnectionsList

  public export
  (.inputsLen) : ModuleSig -> Nat
  (.inputsLen) m = length m.inputs

  public export
  (.outputsLen) : ModuleSig -> Nat
  (.outputsLen) m = length m.outputs

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

namespace FinsList

  public export
  data FinsList : Nat -> Type where
    Nil  : FinsList n
    (::) : Fin n -> FinsList n -> FinsList n

  %name FinsList fs

  public export
  (.asList) : FinsList n -> List (Fin n)
  (.asList) []      = []
  (.asList) (x::xs) = x :: xs.asList

  public export
  (.length) : FinsList n -> Nat
  (.length) [] = 0
  (.length) (x::xs) = S xs.length

public export
totalInputs : {ms : ModuleSigsList} -> FinsList ms.length -> Nat
totalInputs []      = 0
totalInputs (i::is) = (index ms i).inputsLen + totalInputs is

public export
totalOutputs : {ms : ModuleSigsList} -> FinsList ms.length -> Nat
totalOutputs []      = 0
totalOutputs (i::is) = (index ms i).outputsLen + totalOutputs is

-- equivalent of `Vect outs (Fin ins)`
-- Each output has a connection from some single input.
-- Each input can go to several outputs.
public export
data Connections : (ins, outs : Nat) -> Type where
  Nil  : Connections ints Z
  (::) : Fin ins -> Connections ins outs -> Connections ins (S outs)

public export
data Modules : ModuleSigsList -> Type where

  End : Modules ms

  -- module containing only of submodules and connections
  NewCompositeModule :
    (m : ModuleSig) ->
    (subMs : FinsList ms.length) ->
    (conn : Connections (m.inputsLen + totalOutputs {ms} subMs) (m.outputsLen + totalInputs {ms} subMs)) ->
    (cont : Modules (m::ms)) ->
    Modules ms

export
genModules : Fuel -> (ms : ModuleSigsList) -> Gen MaybeEmpty $ Modules ms
