module Test.Common.PrintableDesigns

import Data.Vect
import Data.List
import Data.Fuel
import Data.Fin.Split

import Test.Common.DataType
import public Test.Common.Design
import Test.Common.UniqueNames

import Text.PrettyPrint.Bernardy

%default total

export
defaultIndent : {opts : _} -> Doc opts -> Doc opts
defaultIndent = indent 2

export
emptyLine : {opts : _} -> Doc opts
emptyLine = line ""

||| For standard gates in SystemVerilog only position-based connections are allowed.
||| For user modules, interfaces, primitives and prograusl both position-based and name-based connections are allowed.
||| This type stores the names of inputs and outputs, if they exist
public export
data PortNames : Nat -> Type where
  StdModule  : (plen : Nat) -> PortNames plen
  UserModule : (names : Vect plen String) -> PortNames plen

public export
record PrintableDesign portsCnt where
  constructor MkPrintableDesign
  name    : String
  portNames : PortNames portsCnt

namespace PrintableDesigns

  public export
  data PrintableDesigns : (l : Lang) -> (usl : DesignUnitSigsList l) -> Type where
    Nil  : PrintableDesigns l []
    (::) : PrintableDesign m.ports.length -> PrintableDesigns l usl -> PrintableDesigns l (m :: usl)

  public export
  length : PrintableDesigns l usl -> Nat
  length [] = Z
  length (l :: ls) = S $ length ls

  public export %inline
  (.length) : PrintableDesigns _ _ -> Nat
  (.length) = length

  public export
  index : {usl : _} -> (ps : PrintableDesigns l usl) -> (fin: Fin usl.length) ->
          PrintableDesign ((index usl fin).ports.length)
  index (m::_ ) FZ     = m
  index (_::usl) (FS i) = index usl i

public export
allDesignNames : PrintableDesigns l usl -> SVect usl.length
allDesignNames []        = []
allDesignNames (x :: xs) = x.name :: allDesignNames xs

export
genPDNames : {lk : Nat} -> (keywords : SVect lk) -> Fuel -> {l : _} -> {usl : _} -> (pds : PrintableDesigns l usl) ->
             {un : UniqNames usl.length (allDesignNames pds)} ->
             {s : _} -> {subUs : _} -> {mcs : _} ->
             DesignUnit {l} s usl subUs mcs ->
             Gen0 $ (unitName  : String
                 ** connNames : SVect (length mcs)
                 ** subNames  : SVect (subUs.length)
                --  ** allNames : SVect ((subUs.length) + ((length mcs) + (S (length usl))))
                 ** uniqueDes : UniqNames (S (length usl)) (unitName :: allDesignNames pds)
                 ** UniqNames (subUs.length + (length mcs + (S $ length usl))) (subNames ++ connNames ++ unitName :: allDesignNames pds))
genPDNames kw x pds (MkDesign s subUs mcs) = do
  -- Generate design unit name
  (unitName ** uniqueDes) <- genOneUniqueName kw x (allDesignNames pds) un
  let allDesEntNames : ?
      allDesEntNames = unitName :: allDesignNames pds

  -- Generate connections names
  (connNames ** uniqueConns) <- genNUniqueNames kw x (length mcs) allDesEntNames uniqueDes
  let allDesEntConNames : ?
      allDesEntConNames = connNames ++ allDesEntNames

  -- Generate submodule instance names
  (subNames ** uniqueConnsSubs) <- genNUniqueNames kw x (subUs.length) allDesEntConNames uniqueConns

  pure (unitName ** connNames ** subNames ** uniqueDes ** uniqueConnsSubs)

export
printIt : (n : Nat) -> (Fin n -> Gen0 $ Doc opts) -> Gen0 $ List $ Doc opts
printIt n f = traverse f $ List.allFins n

export
forEachTopPort : {opts : _} ->
                 {l : _} -> {s : DesignUnitSig l} -> {usl : DesignUnitSigsList l} -> {subUs : FinsList usl.length} ->
                 (mcs : MultiConnectionsList l s usl subUs) ->
                 (topNames : Vect (s.portsCnt) String) ->
                 (DataType l -> String -> PortMode l -> Gen0 $ Doc opts) ->
                 Gen0 $ List $ Doc opts
forEachTopPort {s} mcs topNames f =
  printIt (totalTops' s) $ \portFin => case findTypeTop portFin mcs of
    Nothing => pure $ line "(error: top port not found in mcs)"
    Just dt => f dt (index portFin topNames) (topPortMode s portFin)

public export
toTotalSubsIdx : {usl : DesignUnitSigsList l} ->
                 {subUs : FinsList usl.length} ->
                 (idx : Fin subUs.length) ->
                 Fin (index usl (index subUs idx)).ports.length ->
                 Fin (totalSubs' usl subUs)
toTotalSubsIdx {usl} {subUs = (f::fs)} idx portNum with 0 (sym $ pslistLen (index usl f).ports (totalSubs usl fs))
                                                      | 0 (length $ (index usl f).ports ++ totalSubs usl fs)
  toTotalSubsIdx FZ       portNum | Refl | _ = indexSum $ Left portNum
  toTotalSubsIdx (FS idx) portNum | Refl | _ = indexSum $ Right $ toTotalSubsIdx idx portNum


export
isElem : Eq a => (x : a) -> (xs : List a) -> Bool
isElem x []      = False
isElem x (y::xs) = case x == y of
  True => True
  False => isElem x xs

export
isSubPortOf : Fin (totalSubs' usl subUs) -> (mcs : MultiConnectionsList l s usl subUs) -> Maybe $ Fin $ length mcs
isSubPortOf f mcs = findIndex resolve $ toVect mcs where
  resolve : MultiConnection l s usl subUs -> Bool
  resolve (MkMC tsc tsk ssc ssk) = isElem f ssc.asList || isElem f ssk.asList

export
findSubPortName : {mcs : MultiConnectionsList l s usl subUs} -> {mcsNames : Vect (length mcs) String} -> Fin (totalSubs' usl subUs) -> String
findSubPortName f = case isSubPortOf f mcs of
  Nothing   => "(error: findSubPortName \{show $ finToNat f})"
  Just mcsF => index mcsF mcsNames

export
eqmf : MFin x -> Fin x -> Bool
eqmf Nothing  f' = False
eqmf (Just f) f' = f == f'

||| Returns top inputs and outputs names
export
resolveInpsOutsNames : {l : _} -> {usl : _} -> {s : _} -> {subUs : _} -> {mcs : _} ->
                       DesignUnit {l} s usl subUs mcs -> Vect (length mcs) String -> (Vect (length $ s.ports) String)
resolveInpsOutsNames (MkDesign s subUs mcs) mcsNames = map findName $ allFins (length $ s.ports) where
  mfinToName : Maybe (Fin $ length mcs) -> String
  mfinToName Nothing  = "(error: resolveInpsOutsNames nothing portsLen: \{show $ length $ s.ports})" -- impossible
  mfinToName (Just f) = index f mcsNames

  findName : Fin (totalTops' s) -> String
  findName f = mfinToName $ findIndex (\(MkMC tsc tsk ssc ssk) => eqmf tsc f || eqmf tsk f) $ toVect mcs
