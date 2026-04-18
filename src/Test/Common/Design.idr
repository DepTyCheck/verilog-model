module Test.Common.Design

import Data.Fin
import Data.Fuel
import Data.Vect

import Test.DepTyCheck.Gen

import public Test.Common.Utils
import public Test.Common.DataType

%default total

namespace Port

  public export
  record Port l where
    constructor MkPort
    type  : DataType l
    mode  : PortMode l
    {auto 0 prf : AllowedPort type mode}

  %name Port p

  public export
  data PortsList : Lang -> Type where
    Nil  : PortsList l
    (::) : Port l -> PortsList l -> PortsList l

  %name PortsList ps

  public export
  length : PortsList l -> Nat
  length []      = Z
  length (_::ps) = S $ length ps

  public export %inline
  (.length) : PortsList l -> Nat
  (.length) = length

  public export
  index : (ps : PortsList l) -> Fin ps.length -> Port l
  index (p::_ ) FZ     = p
  index (_::ps) (FS i) = index ps i

  public export
  (++) : PortsList l -> PortsList l -> PortsList l
  Nil       ++ ys = ys
  (x :: xs) ++ ys = x :: (xs ++ ys)

  export
  pslistLen : {0 l : _} -> (xs : PortsList l) -> (ys : PortsList l) -> xs.length + ys.length = (xs ++ ys).length
  pslistLen []        ys = Refl
  pslistLen (_ :: xs) ys = rewrite pslistLen xs ys in Refl

  export
  sympslistLen : {0 l : _} -> {0 a, b : PortsList l} -> (0 m : Nat -> Type) -> m (a.length + b.length) -> m ((a ++ b).length)
  sympslistLen _ v = rewrite sym $ pslistLen a b in v

  export
  fixPSVect : {0 a, b: PortsList l} -> Vect (a.length + b.length) c -> Vect ((a ++ b).length) c
  fixPSVect = sympslistLen $ \n => Vect n c

  export
  fixPSFin : {0 l : _} -> {0 a, b : PortsList l} -> Fin (a.length + b.length) -> Fin ((a ++ b).length)
  fixPSFin = sympslistLen Fin

namespace DesignUnitSig

  public export
  record DesignUnitSig l where
    constructor MkDesignUnitSig
    ports : PortsList l

  public export
  (.portsCnt) : DesignUnitSig l -> Nat
  (.portsCnt) = .ports.length

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

  public export
  totalTops : DesignUnitSig l -> PortsList l
  totalTops = .ports

  public export
  totalTops' : DesignUnitSig l -> Nat
  totalTops' s = length $ totalTops s

  public export
  totalSubs : (usl : DesignUnitSigsList l) -> (subUs : FinsList usl.length) -> PortsList l
  totalSubs usl []      = []
  totalSubs usl (f::fs) = (totalTops $ index usl f) ++ totalSubs usl fs

  public export
  totalSubs' : (usl : DesignUnitSigsList l) -> (subUs : FinsList usl.length) -> Nat
  totalSubs' usl subUs = length $ totalSubs usl subUs

  public export
  subPort : (usl : DesignUnitSigsList l) -> (subUs : FinsList usl.length) -> Fin (totalSubs' usl subUs) -> Port l
  subPort usl subUs = index $ totalSubs usl subUs

  public export
  topPortType : (s : DesignUnitSig l) -> Fin (totalTops' s) -> DataType l
  topPortType s f = (index (totalTops s) f).type

  public export
  subPortType : (usl : DesignUnitSigsList l) -> (subUs : FinsList usl.length) -> Fin (totalSubs' usl subUs) -> DataType l
  subPortType usl subUs f = (subPort usl subUs f).type

  public export
  topPortMode : (s : DesignUnitSig l) -> Fin (totalTops' s) -> PortMode l
  topPortMode s f = (index (totalTops s) f).mode

  public export
  subPortMode : (usl : DesignUnitSigsList l) -> (subUs : FinsList usl.length) -> Fin (totalSubs' usl subUs) -> PortMode l
  subPortMode usl subUs f = (subPort usl subUs f).mode

  public export
  writePortModeSV : SVPortMode -> Bool
  writePortModeSV In    = False
  writePortModeSV Out   = True
  writePortModeSV InOut = True
  -- writePortModeSV Ref   = False

  public export
  writePortModeVHDL : VHDLPortMode -> Bool
  writePortModeVHDL In      = False
  writePortModeVHDL Out     = True
  writePortModeVHDL InOut   = True
  writePortModeVHDL Buffer  = True
  writePortModeVHDL Linkage = False

  public export
  writePortMode : PortMode l -> Bool
  writePortMode (SVP x) = writePortModeSV x
  writePortMode (VHP x) = writePortModeVHDL x

  public export
  readPortMode : PortMode l -> Bool
  readPortMode = not . writePortMode

  public export
  isTopSource : {s : _} -> (Fin $ totalTops' s) -> Bool
  isTopSource f = readPortMode $ topPortMode s f

  public export
  isSubSource : {usl : DesignUnitSigsList l} -> {subUs : FinsList $ usl.length} ->  (Fin $ totalSubs' usl subUs) -> Bool
  isSubSource f = writePortMode $ subPortMode usl subUs f


namespace MultiConnection

  public export
  data MCNotEmpty : MFin a -> MFin a -> FinsList b -> FinsList b -> Type where
    JustTSC : MCNotEmpty (Just x) tsk ssc ssk
    JustTSK : MCNotEmpty tsc (Just x) ssc ssk
    JustSSC : MCNotEmpty tsc tsk (x :: xs) ssk
    JustSSK : MCNotEmpty tsc tsk ssc (x :: xs)

  public export
  data OnlyOneTop : MFin a -> MFin a -> Type where
    OnlyTSC : OnlyOneTop (Just x) Nothing
    OnlyTSK : OnlyOneTop Nothing (Just x)
    NoTop   : OnlyOneTop Nothing Nothing

  public export
  data MultiConnection : (l : Lang) -> (s : DesignUnitSig l) ->
                         (usl : DesignUnitSigsList l) -> (subUs : FinsList usl.length) -> Type where
    MkMC : {l : _} -> {s : _} -> {usl : _} -> {subUs : _} ->
           (tsc : MFin (totalTops' s)) -> (tsk : MFin (totalTops' s)) ->
           (ssc : FinsList $ totalSubs' usl subUs) -> (ssk : FinsList $ totalSubs' usl subUs) ->
           (ne  : MCNotEmpty tsc tsk ssc ssk) =>
           (oot : OnlyOneTop tsc tsk) =>
           MultiConnection l s usl subUs

  public export
  data MultiConnectionsList : (l : Lang) -> (s : DesignUnitSig l) ->
                              (usl : DesignUnitSigsList l) -> (subUs : FinsList usl.length) -> Type where
    Nil  : MultiConnectionsList l s usl subUs
    (::) : MultiConnection l s usl subUs -> MultiConnectionsList l s usl subUs -> MultiConnectionsList l s usl subUs

  public export
  length : MultiConnectionsList l s usl subUs -> Nat
  length []       = Z
  length (_::mcs) = S $ length mcs

  public export %inline
  (.length) : MultiConnectionsList l s usl subUs -> Nat
  (.length) = length

  public export
  index : (mcs : MultiConnectionsList l s usl subUs) -> Fin (length mcs) -> MultiConnection l s usl subUs
  index (mc::_ ) FZ     = mc
  index (_::mcs) (FS i) = index mcs i

  public export
  toVect : (mcs : MultiConnectionsList l s usl subUs) -> Vect (length mcs) $ MultiConnection l s usl subUs
  toVect []         = []
  toVect (m :: mcs) = m :: toVect mcs

  public export
  fromList : List (MultiConnection l s usl subUs) -> MultiConnectionsList l s usl subUs
  fromList []        = []
  fromList (x :: xs) = x :: fromList xs

  public export
  rawTypeOf : {l : _} -> {s : _} -> {usl : _} -> {subUs : _} ->
              MultiConnection l s usl subUs -> DataType l
  rawTypeOf (MkMC (Just x) tsk ssc ssk  {ne = JustTSC}) = (index (totalTops s) x).type
  rawTypeOf (MkMC tsc (Just x) ssc ssk  {ne = JustTSK}) = (index (totalTops s) x).type
  rawTypeOf (MkMC tsc tsk (x :: xs) ssk {ne = JustSSC}) = (index (totalSubs usl subUs) x).type
  rawTypeOf (MkMC tsc tsk ssc (x :: xs) {ne = JustSSK}) = (index (totalSubs usl subUs) x).type

  public export
  isTop : MultiConnection l s usl subUs -> Bool
  isTop (MkMC (Just x) Nothing ssc ssk @{ne} @{OnlyTSC}) = True
  isTop (MkMC Nothing (Just x) ssc ssk @{ne} @{OnlyTSK}) = True
  isTop (MkMC Nothing Nothing  ssc ssk @{ne} @{NoTop})   = False

  public export
  typeOf : {l : _} -> {s : _} -> {usl : _} -> {subUs : _} ->
           MultiConnection l s usl subUs -> DataType l
  typeOf {l = SystemVerilog} mc = if isTop mc then rawTypeOf mc else case isUnpacked (dtToSVt $ rawTypeOf mc) of
    True  => rawTypeOf mc
    False => SVT defaultNetType
  typeOf {l = VHDL}          mc = rawTypeOf mc

  ||| Find type of port by fin
  public export
  findTypeTop : {l : _} -> {s : _} -> {usl : _} -> {subUs : _} ->
                Fin (totalTops' s) -> (mcs : MultiConnectionsList l s usl subUs) -> Maybe $ DataType l
  findTypeTop f []                                                           = Nothing
  findTypeTop f (mc@(MkMC (Just x) Nothing ssc ssk @{ne} @{OnlyTSC}) :: mcs) = case f == x of
    False => findTypeTop f mcs
    True  => Just $ typeOf mc
  findTypeTop f (mc@(MkMC Nothing (Just x) ssc ssk @{ne} @{OnlyTSK}) :: mcs) = case f == x of
    False => findTypeTop f mcs
    True  => Just $ typeOf mc
  findTypeTop f (   (MkMC Nothing Nothing  ssc ssk @{ne} @{NoTop})   :: mcs) = findTypeTop f mcs

namespace SystemVerilogRules

  public export
  isMDSV : SVObject -> Bool
  isMDSV (Net Supply0' t) = True
  isMDSV (Net Supply1' t) = True
  isMDSV (Net Triand'  t) = True
  isMDSV (Net Trior'   t) = True
  isMDSV (Net Trireg'  t) = True
  isMDSV (Net Tri0'    t) = True
  isMDSV (Net Tri1'    t) = True
  isMDSV (Net Tri'     t) = True
  isMDSV (Net Wire'    t) = True
  isMDSV (Net Wand'    t) = True
  isMDSV (Net Wor'     t) = True
  isMDSV _                = False

  public export
  isSD : SVObject -> Bool
  isSD (Var st)        = True
  isSD (Net Uwire' st) = True
  isSD _               = False

  public export
  extractBasic : SVObject -> SVType
  extractBasic (Net x t) = t
  extractBasic (Var   t) = t

  public export
  basicIntegral : SVType -> Bool
  basicIntegral (RVar x)            = False
  basicIntegral (AVar x)            = True
  basicIntegral (VVar x)            = True
  basicIntegral (PackedArr   t _ _) = True
  basicIntegral (UnpackedArr x _ _) = basicIntegral x

  public export
  isVarOrPacked' : SVType -> Bool
  isVarOrPacked' (RVar _)            = True
  isVarOrPacked' (AVar _)            = True
  isVarOrPacked' (VVar _)            = True
  isVarOrPacked' (PackedArr   _ _ _) = True
  isVarOrPacked' (UnpackedArr _ _ _) = False

  ||| 6.22.2 Equivalent types
  ||| d) Unpacked fixed-size array types are equivalent if they have equivalent element types and equal size; the actual range bounds may differ.
  ||| IEEE 1800-2023
  public export
  sameUnpackedDims : SVType -> SVType -> Bool
  sameUnpackedDims (UnpackedArr t s e) (UnpackedArr t' s' e') =
    S (max s e `minus` min s e) == S (max s' e' `minus` min s' e')
    && sameUnpackedDims t t'
  sameUnpackedDims (UnpackedArr _ _ _) _ = False
  sameUnpackedDims _ (UnpackedArr _ _ _) = False
  sameUnpackedDims _ _                   = True

  ||| 6.22.2 Equivalent types
  ||| c) Packed arrays, packed structures, packed unions, and built-in integral types are equivalent if they
  ||| contain the same number of total bits, are either all 2-state or all 4-state, and are either all signed or
  ||| all unsigned.
  ||| NOTE — If any bit of a packed structure or union is 4-state, the entire structure or union is considered 4-state.
  public export
  equivalentSVT : SVType -> SVType -> Bool
  equivalentSVT t t' = bitsCnt t == bitsCnt t'
                   && states t == states t'
                   && isSigned t == isSigned t'
                   && basicIntegral t == basicIntegral t'
                   && isVarOrPacked' t == isVarOrPacked' t'
                   && sameUnpackedDims t t'

  public export
  equivalentSVO : SVObject -> SVObject -> Bool
  equivalentSVO a b = equivalentSVT (extractBasic a) (extractBasic b)

  public export
  isVarOrPacked : SVObject -> Bool
  isVarOrPacked = isVarOrPacked' . extractBasic

  public export
  typesCompatibleSV : (mc : DataType SystemVerilog) -> (p : DataType SystemVerilog) -> Bool
  typesCompatibleSV mc p with (isVarOrPacked $ dtToSVt mc) | (isVarOrPacked $ dtToSVt p)
    typesCompatibleSV mc p | False | False = equivalentSVO (dtToSVt mc) (dtToSVt p)
    typesCompatibleSV mc p | False | True  = False
    typesCompatibleSV mc p | True  | False = False
    typesCompatibleSV mc p | True  | True  = True


  ||| 23.3.3.2 Port connection rules for variables
  ||| A variable data type is not permitted on either side of an inout port.
  |||
  ||| IEEE 1800-2023
  public export
  forbidVarInout : SVPortMode -> DataType SystemVerilog -> SVPortMode -> DataType SystemVerilog -> Bool
  forbidVarInout formal formalT actual actualT =
    (formal /= InOut && actual /= InOut)
    || (not (isVar $ dtToSVt formalT) && not (isVar $ dtToSVt actualT))

  -- ||| 23.3.3.2 Port connection rules for variables
  -- |||  — A ref port shall be connected to an equivalent variable data type. References to the port variable
  -- |||  shall be treated as hierarchical references to the variable to which it is connected in its instantiation.
  -- |||  This kind of port cannot be left unconnected. See 6.22.2.
  -- |||
  -- ||| IEEE 1800-2023
  -- public export
  -- requireEqForRef : SVPortMode -> DataType SystemVerilog -> SVPortMode -> DataType SystemVerilog -> Bool
  -- requireEqForRef formal formalT actual actualT = if actual == Ref && formal == Ref then equivalentSVO (dtToSVt formalT) (dtToSVt actualT) else True

  |||  23.3.3.2 Port connection rules for variables
  |||  If a port declaration has a variable data type, then its direction controls how it can be connected when
  |||  instantiated, as follows:
  |||  — An input port can be connected to any expression of a compatible data type. A continuous
  |||  assignment shall be implied when a variable is connected to an input port declaration. Assignments
  |||  to variables declared as input ports shall be illegal. If left unconnected, the port shall have the default
  |||  initial value corresponding to the data type.
  |||  — An output port can be connected to a variable (or a concatenation) of a compatible data type. A
  ||| continuous assignment shall be implied when a variable is connected to the output port of an
  |||  instance. Procedural or continuous assignments to a variable connected to the output port of an
  |||  instance shall be illegal.
  |||  — An output port can be connected to a net (or a concatenation) of a compatible data type. In this
  |||  case, multiple drivers shall be permitted on the net.
  |||  — A variable data type is not permitted on either side of an inout port.
  |||  — A ref port shall be connected to an equivalent variable data type. References to the port variable
  |||  shall be treated as hierarchical references to the variable to which it is connected in its instantiation.
  |||  This kind of port cannot be left unconnected. See 6.22.2.
  |||  — It shall be illegal to connect a port variable to an interconnect port or interconnect net.
  |||
  |||  23.3.3.3 Port connection rules for nets with built-in net types
  |||  If a port declaration has a net type, such as wire, then its direction controls how it can be connected, as
  |||  follows:
  |||  — An input can be connected to any expression of a compatible data type. If left unconnected, it shall
  |||  have the value 'z.
  |||  — An output can be connected to a net or variable (or a concatenation of nets or variables) of a
  |||  compatible data type.
  |||  — An inout can be connected to a net (or a concatenation of nets) of a compatible data type or left
  |||  unconnected, but cannot be connected to a variable.
  |||
  ||| IEEE 1800-2023
  ||| Sub-to-sub port-mode rules (no top-port context involved).
  public export
  subPmsSV : (formal : PortMode SystemVerilog) -> (formalT : DataType SystemVerilog) ->
             (actual : PortMode SystemVerilog) -> (actualT : DataType SystemVerilog) -> Bool
  subPmsSV (SVP formal) formalT (SVP actual) actualT = forbidVarInout formal formalT actual actualT
                                                   -- && requireEqForRef formal formalT actual actualT

  ||| 23.3.3.2 Port connection rules for variables
  |||
  ||| Assignments to variables declared as input ports shall be illegal.
  |||
  ||| IEEE 1800-2023
  public export
  isTopInputVar : (s : DesignUnitSig SystemVerilog) -> Fin (totalTops' s) -> Bool
  isTopInputVar s x = topPortMode s x == SVP In && isVar (dtToSVt $ topPortType s x)

  public export
  pmsSV : {s : _} -> {usl : _} -> {subUs : _} ->
          (mc : MultiConnection SystemVerilog s usl subUs) -> (f : Fin (totalSubs' usl subUs)) ->
          PortMode SystemVerilog -> DataType SystemVerilog ->
          PortMode SystemVerilog -> DataType SystemVerilog -> Bool
  pmsSV (MkMC (Just x) Nothing _ _ @{ne} @{OnlyTSC}) f formal formalT actual actualT =
       subPmsSV formal formalT actual actualT
    && not (isTopInputVar s x && isSubSource {usl} {subUs} f)
  pmsSV _ _ formal formalT actual actualT = subPmsSV formal formalT actual actualT


namespace VHDLRules

  ||| For actual Linkage only Linkage formal is allowed
  public export
  checkLinkage : VHDLPortMode -> VHDLPortMode -> Bool
  checkLinkage Linkage Linkage = True
  checkLinkage In      Linkage = False
  checkLinkage Out     Linkage = False
  checkLinkage InOut   Linkage = False
  checkLinkage Buffer  Linkage = False
  checkLinkage _       _       = True

  ||| 6.5.6.3 Port clauses
  |||
  ||| if a formal signal port is associated with
  ||| an actual that is itself a port, then the following restrictions apply depending upon the mode (see 6.5.2), if
  ||| any, of the formal signal port:
  ||| a) For a formal signal port of mode in the associated actual shall be a port of mode in, out, inout, or
  ||| buffer. This restriction applies both to an actual that is associated as a name in the actual part of an
  ||| association element and to an actual that is associated as part of an expression in the actual part of an
  ||| association element.
  ||| b) For a formal signal port of mode out, the associated actual shall be a port of mode out, inout, or
  ||| buffer.
  ||| c) For a formal signal port of mode inout, the associated actual shall be a port of mode out, inout, or
  ||| buffer.
  ||| d) For a formal signal port of mode buffer, the associated actual shall be a port of mode out, inout, or
  ||| buffer.
  ||| e) For a formal signal port of mode linkage, the associated actual may be a port of any mode.
  |||
  ||| IEEE 1076-2019
  -- The logic corresponds to material conditional (. . )
  public export
  checkDirectionVHDL : VHDLPortMode -> VHDLPortMode -> Bool
  checkDirectionVHDL formal actual with (writePortModeVHDL formal)
    checkDirectionVHDL formal actual | False = True -- formal is in or linkage
    checkDirectionVHDL formal actual | True  = case writePortModeVHDL actual of
      False => False -- write to read is prohibited
      True  => True  -- write to write is allowed

  public export
  subPmsVHDL : (formal : PortMode VHDL) -> (formalT : DataType VHDL) ->
               (actual : PortMode VHDL) -> (actualT : DataType VHDL) -> Bool
  subPmsVHDL _ _ _ _ = True

  public export
  pmsVHDL : PortMode VHDL -> DataType VHDL -> PortMode VHDL -> DataType VHDL -> Bool
  pmsVHDL (VHP formal) _ (VHP actual) _ = checkDirectionVHDL formal actual
                                       && checkLinkage formal actual

  public export
  isMDVHDL : VHDLType -> Bool
  isMDVHDL StdLogic = True
  isMDVHDL _        = False

  public export
  typesCompatibleVHDL : (mc : DataType VHDL) -> (p : DataType VHDL) -> Bool
  typesCompatibleVHDL mc p = dtToVHt mc == dtToVHt p

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
  newTop f with (isTopSource f)
    newTop f | False = MkMC Nothing (Just f) [] []
    newTop f | True  = MkMC (Just f) Nothing [] []

  public export
  newTop' : {l : _} -> {s : _} -> {usl : _} -> {subUs : _} ->
           Nat -> MultiConnection l s usl subUs
  newTop' k = case natToFin' k $ totalTops' s of
    Nothing     => assert_total $ idris_crash "newTop': k out of bounds for totalSubs'"
    (Just topF) => newTop topF

  public export
  newSub : {l : _} -> {s : _} -> {usl : _} -> {subUs : _} ->
           Fin (totalSubs' usl subUs) -> MultiConnection l s usl subUs
  newSub f with (isSubSource f)
    newSub f | False = MkMC Nothing Nothing [] [ f ]
    newSub f | True  = MkMC Nothing Nothing [ f ] []

  public export
  newSub' : {l : _} -> {s : _} -> {usl : _} -> {subUs : _} ->
           Nat -> MultiConnection l s usl subUs
  newSub' k = case natToFin' k $ totalSubs' usl subUs of
    Nothing     => assert_total $ idris_crash "newSub': k out of bounds for totalSubs'"
    (Just subF) => newSub subF

  public export
  addSub : {l : _} -> {s : _} -> {usl : _} -> {subUs : _} ->
           Fin (totalSubs' usl subUs) -> MultiConnection l s usl subUs ->  MultiConnection l s usl subUs
  addSub f mc with (isSubSource f)
    addSub f (MkMC tsc tsk ssc ssk) | False = MkMC tsc tsk ssc (f::ssk)
    addSub f (MkMC tsc tsk ssc ssk) | True  = MkMC tsc tsk (f::ssc) ssk

  public export
  addSub' : {l : _} -> {s : _} -> {usl : _} -> {subUs : _} ->
           Nat -> MultiConnection l s usl subUs -> MultiConnection l s usl subUs
  addSub' k mc = case natToFin' k $ totalSubs' usl subUs of
    Nothing     => assert_total $ idris_crash "addSub': k out of bounds for totalSubs'"
    (Just subF) => addSub subF mc

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
  buildTopMCS : (l : Lang) -> (s : DesignUnitSig l) ->
                (usl : DesignUnitSigsList l) -> (subUs : FinsList usl.length) -> MultiConnectionsList l s usl subUs
  buildTopMCS _ s _ _ = fromList $ map newTop $ List.allFins $ totalTops' s

  public export
  typesCompatible : {l : _} -> (mc : DataType l) -> (p : DataType l) -> Bool
  typesCompatible {l = SystemVerilog} = typesCompatibleSV
  typesCompatible {l = VHDL}          = typesCompatibleVHDL

  public export
  checkSubPMs : {l : Lang} ->
               (formal : PortMode l) -> (formalT : DataType l) -> (actual : PortMode l) -> (actualT : DataType l) -> Bool
  checkSubPMs {l = SystemVerilog} = subPmsSV
  checkSubPMs {l = VHDL}          = subPmsVHDL

  public export
  checkPortModes : {l : _} -> {s : _} -> {usl : _} -> {subUs : _} ->
                   (mc : MultiConnection l s usl subUs) -> (f : Fin (totalSubs' usl subUs)) ->
                   PortMode l -> DataType l -> PortMode l -> DataType l -> Bool
  checkPortModes {l = SystemVerilog} mc f = pmsSV mc f
  checkPortModes {l = VHDL}          _ _  = pmsVHDL

  public export
  portModesCompatible : {l : _} -> {s : _} -> {usl : _} -> {subUs : _} ->
                        MultiConnection l s usl subUs -> Fin (totalSubs' usl subUs) -> Bool
  portModesCompatible mc@(MkMC (Just x) Nothing ssc ssk @{ne} @{OnlyTSC}) f = checkPortModes mc f
                                                                                             (subPortMode usl subUs f)
                                                                                             (subPortType usl subUs f)
                                                                                             (topPortMode s x)
                                                                                             (typeOf mc)
  portModesCompatible mc@(MkMC Nothing (Just x) ssc ssk @{ne} @{OnlyTSK}) f = checkPortModes mc f
                                                                                             (subPortMode usl subUs f)
                                                                                             (subPortType usl subUs f)
                                                                                             (topPortMode s x)
                                                                                             (typeOf mc)
  portModesCompatible mc@(MkMC Nothing Nothing  (x::xs) _ @{JustSSC} @{NoTop})   f = checkSubPMs (subPortMode usl subUs f)
                                                                                                 (subPortType usl subUs f)
                                                                                                 (subPortMode usl subUs x)
                                                                                                 (typeOf mc)
  portModesCompatible mc@(MkMC Nothing Nothing  _ (x::xs) @{JustSSK} @{NoTop})   f = checkSubPMs (subPortMode usl subUs f)
                                                                                                 (subPortType usl subUs f)
                                                                                                 (subPortMode usl subUs x)
                                                                                                 (typeOf mc)

  public export
  isResolved : DataType l -> Bool
  isResolved (SVT o) = isMDSV o
  isResolved (VHD t) = isMDVHDL t

  public export
  noSource : {s : _} -> {usl : _} -> {subUs : _} -> MultiConnection l s usl subUs -> Bool
  noSource (MkMC Nothing tsk []        ssk) = True
  noSource (MkMC Nothing tsk (x :: fs) ssk) = False
  noSource (MkMC (Just x) tsk ssc      ssk) = False

  public export
  canDrive : {l : _} -> {s : _} -> {usl : _} -> {subUs : _} ->
             MultiConnection l s usl subUs -> Fin (totalSubs' usl subUs) -> Bool
  canDrive mc f with ((not $ isSubSource {usl} {subUs} f) || (isResolved $ typeOf mc) || noSource mc) -- f is sink or mc is multidriven or has no source
    canDrive (MkMC Nothing tsk []  ssk) f | False = True
    canDrive (MkMC tsc     tsk ssc ssk) f | False = False
    canDrive mc                         f | True  = True

  public export
  isGoodMC : {l : _} -> {s : _} -> {usl : _} -> {subUs : _} ->
             MultiConnection l s usl subUs -> Fin (totalSubs' usl subUs) -> Bool
  isGoodMC mc subF = typesCompatible (typeOf mc) (subPortType usl subUs subF)
                  && canDrive mc subF
                  && portModesCompatible mc subF

  public export
  filterGoodMCs : {l : _} -> {s : _} -> {usl : _} -> {subUs : _} ->
                  (mid : MultiConnectionsList l s usl subUs) -> List (Fin $ length mid) -> Fin (totalSubs' usl subUs) -> FinsList (length mid)
  filterGoodMCs mid []      subF = []
  filterGoodMCs mid (f::fs) subF = case isGoodMC (index mid f) subF of
    False => filterGoodMCs mid fs subF
    True  => f :: filterGoodMCs mid fs subF

  public export
  filterGoodMCs' : {l : _} -> {s : _} -> {usl : _} -> {subUs : _} ->
                   (mid : MultiConnectionsList l s usl subUs) -> List (Fin $ length mid) -> Nat -> FinsList (length mid)
  filterGoodMCs' mid fins k = case natToFin' k $ totalSubs' usl subUs of
    Nothing     => [] -- impossible
    (Just subF) => filterGoodMCs mid fins subF

  public export
  goodFins : {l : _} -> {s : _} -> {usl : _} -> {subUs : _} ->
             (mid : MultiConnectionsList l s usl subUs) -> Nat -> FinsList (length mid)
  goodFins mid k = filterGoodMCs' mid (List.allFins $ length mid) k

  public export
  insertAt : {l : _} -> {s : _} -> {usl : _} -> {subUs : _} ->
             (mid : MultiConnectionsList l s usl subUs) -> (k : Nat) -> (f : Fin (goodFins mid k).length) -> MultiConnectionsList l s usl subUs
  insertAt mid k f = replaceAt mid indexOfGoodList
                   $ addSub' k
                   $ index mid indexOfGoodList where
    indexOfGoodList : Fin (length mid)
    indexOfGoodList = index (goodFins mid k) f

  public export
  data FillSub : (l : Lang) -> (s : DesignUnitSig l) ->
                 (usl : DesignUnitSigsList l) -> (subUs : FinsList usl.length) ->
                 MultiConnectionsList l s usl subUs -> (subi : Nat) -> MultiConnectionsList l s usl subUs -> Type where
    SEnd      : FillSub l s usl subUs pre 0 pre
    SNew      : (recur : FillSub l s usl subUs pre k mid) ->
                FillSub l s usl subUs pre (S k) $ newSub' k :: mid
    SExisting : (recur : FillSub l s usl subUs pre k mid) ->
                (f : Fin (goodFins mid k).length) ->
                FillSub l s usl subUs pre (S k) $ insertAt mid k f

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
               {0 fs : FillSub l s usl subUs (buildTopMCS l s usl subUs) (totalSubs' usl subUs) mcs} ->
               DesignUnit {l} s usl subUs mcs

  public export
  data DesignUnitsList : DesignUnitSigsList l -> Type where
    Nil  : DesignUnitsList usl
    (::) : {s : _} -> {usl : _} -> {subUs : _} -> {mcs : _} ->
           DesignUnit {l} s usl subUs mcs -> DesignUnitsList {l} (s::usl) -> DesignUnitsList {l} usl

export
genDesignUnitsList : Fuel -> (l : Lang) -> (usl : DesignUnitSigsList l) ->
  Gen MaybeEmpty $ DesignUnitsList usl
