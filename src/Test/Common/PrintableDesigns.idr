module Test.Common.PrintableDesigns

import Data.Vect
import Data.Fuel

import public Test.Common.Design
import Test.Common.UniqueNames


||| For standart gates in SystemVerilog only position-based connections are allowed.
||| For user modules, interfaces, primitives and programs both position-based and name-based connections are allowed.
||| This type stores the names of inputs and outputs, if they exist
public export
data InsOuts : (ins, outs : Nat) -> Type where
  StdModule  : (ins, outs : Nat) -> InsOuts ins outs
  UserModule : (inputs : Vect ins String) -> InsOuts ins outs

public export
record PrintableDesign inps outs where
  constructor MkPrintableDesign
  name    : String
  insOuts : InsOuts inps outs

namespace PrintableDesigns

  public export
  data PrintableDesigns : (l : Lang) -> (ms : DesignUnitSigsList l) -> Type where
    Nil  : PrintableDesigns l []
    (::) : PrintableDesign m.inpsCount m.outsCount -> PrintableDesigns l ms -> PrintableDesigns l (m :: ms)

  public export
  length : PrintableDesigns l ms -> Nat
  length [] = Z
  length (l :: ls) = S $ length ls

  public export %inline
  (.length) : PrintableDesigns _ _ -> Nat
  (.length) = length

  public export
  index : {ms : _} -> (ps : PrintableDesigns l ms) -> (fin: Fin ms.length) -> 
          PrintableDesign ((index ms fin).inpsCount) ((index ms fin).outsCount)
  index (m::_ ) FZ     = m
  index (_::ms) (FS i) = index ms i

public export
allDesignNames : PrintableDesigns l ms -> SVect ms.length
allDesignNames []        = []
allDesignNames (x :: xs) = x.name :: allDesignNames xs

export
genPDNames : {lk : Nat} -> (keywords : SVect lk) -> Fuel -> {l : _} -> {dus : _} -> (pds : PrintableDesigns l dus) ->
             {un : UniqNames dus.length (allDesignNames pds)} ->
             {uu : _} -> {u : _} ->
             (subUs : FinsList uu.length) ->
             (mcs : MultiConnectionsList VHDL uu u subUs) ->
             Gen0 $ (unitName  : String 
                 ** (connNames : SVect (length mcs)
                 ** (subNames  : SVect (subUs.length)
                 ** UniqNames (subUs.length + ((length mcs) + S (length dus))) $ subNames ++ connNames ++ unitName :: allDesignNames pds)))
genPDNames kw x pds {un} subUs mcs = do
  -- Generate design unit name
  (entityName ** uniqueDes) <- genOneUniqueName kw x (allDesignNames pds) un
  let allDesNames : ? 
      allDesNames = entityName :: allDesignNames pds

  -- Generate connections names
  (connNames ** uniqueConns) <- genNUniqueNames kw x (length mcs) allDesNames uniqueDes
  let allDesConnsNames : ? 
      allDesConnsNames = connNames ++ allDesNames

  -- Generate submodule instance names
  (subNames ** uniqueConnsSubs) <- genNUniqueNames kw x (subUs.length) allDesConnsNames uniqueConns
  
  pure (entityName ** connNames ** subNames ** uniqueConnsSubs)


export
genPDNames' : {lk : Nat} -> (keywords : SVect lk) -> Fuel -> {l : _} -> {dus : _} -> (pds : PrintableDesigns l dus) ->
             {un : UniqNames dus.length (allDesignNames pds)} ->
             {uu : _} -> {u : _} ->
             (subUs : FinsList uu.length) ->
             (mcs : MultiConnectionsList VHDL uu u subUs) ->
             Gen0 $ (unitName : String
                  ** connNames : Vect (length mcs) String
                  ** subNames : Vect (subUs .length) String
                  ** allNames : SVect (S ((length mcs) + (subUs .length)))
                  ** UniqNames (S (((length mcs) + (subUs .length)) + (length dus))) (allNames ++ allDesignNames pds))
genPDNames' kw x pds {un} subUs mcs = do
  -- Generate design unit name
  (allNames ** uniqueAll) <- genNUniqueNames kw x (S (length mcs) + (subUs.length)) (allDesignNames pds) un

  let av : ?
      av = toVect allNames

  let unitName  = index 0 $ take 1 av
  let connNames = drop 1 $ take (S $ length mcs) av
  let subNames  = drop (S $ length mcs) av

  pure (unitName ** connNames ** subNames ** allNames ** uniqueAll)
