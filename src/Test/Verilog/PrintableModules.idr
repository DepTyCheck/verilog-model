module Test.Verilog.PrintableModules

import Data.Vect

import public Test.Verilog.Connections
import public Test.Verilog.UniqueNames

||| For standart gates in SystemVerilog only position-based connections are allowed.
||| For user modules, interfaces, primitives and programs both position-based and name-based connections are allowed.
||| This type stores the names of inputs and outputs, if they exist
public export
data InsOuts : (ins, outs : Nat) -> Type where
  StdModule  : (ins, outs : Nat) -> InsOuts ins outs
  UserModule : (inputs : Vect ins String) -> (outputs : Vect outs String) -> InsOuts ins outs

public export
record PrintableModule inps outs where
  constructor MkPrintableModule
  name    : String
  insOuts : InsOuts inps outs

namespace PrintableModules

  public export
  data PrintableModules : (ms : ModuleSigsList) -> Type where
    Nil  : PrintableModules []
    (::) : PrintableModule m.inpsCount m.outsCount -> PrintableModules ms -> PrintableModules (m :: ms)

  public export
  length : PrintableModules _ -> Nat
  length [] = Z
  length (l :: ls) = S $ length ls

  public export %inline
  (.length) : PrintableModules _ -> Nat
  (.length) = length

  public export
  index : {ms : _} -> (ps : PrintableModules ms) -> (fin: Fin ms.length) -> PrintableModule ((index ms fin).inpsCount) ((index ms fin).outsCount)
  index (m::_ ) FZ     = m
  index (_::ms) (FS i) = index ms i

public export
allModuleNames : PrintableModules ms -> SVect ms.length
allModuleNames []        = []
allModuleNames (x :: xs) = x.name :: allModuleNames xs
