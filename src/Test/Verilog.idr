module Test.Verilog

import Data.Fuel
import public Data.Fin

import Test.DepTyCheck.Gen

%default total

namespace ModuleSig

  public export
  record ModuleSig where
    constructor MkModuleSig
    inputs  : Nat
    outputs : Nat

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
totalInputs (i::is) = (index ms i).inputs + totalInputs is

public export
totalOutputs : {ms : ModuleSigsList} -> FinsList ms.length -> Nat
totalOutputs []      = 0
totalOutputs (i::is) = (index ms i).outputs + totalOutputs is

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
    (conn : Connections (m.inputs + totalOutputs {ms} subMs) (m.outputs + totalInputs {ms} subMs)) ->
    (cont : Modules (m::ms)) ->
    Modules ms

export
genModules : Fuel -> (ms : ModuleSigsList) -> Gen MaybeEmpty $ Modules ms
