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
  subPortType : (usl : DesignUnitSigsList l) -> (subUs : FinsList usl.length) -> Fin (totalSubs' usl subUs) -> DataType l
  subPortType usl subUs f = (subPort usl subUs f).type

  public export
  topPortMode : (s : DesignUnitSig l) -> Fin (totalTops' s) -> PortMode l
  topPortMode s f = (index (totalTops s) f).mode

  public export
  subPortMode : (usl : DesignUnitSigsList l) -> (subUs : FinsList usl.length) -> Fin (totalSubs' usl subUs) -> PortMode l
  subPortMode usl subUs f = (subPort usl subUs f).mode

  public export
  writePortModeVHDL : VHDLPortMode -> Bool
  writePortModeVHDL In      = False
  writePortModeVHDL Out     = True
  writePortModeVHDL InOut   = True
  writePortModeVHDL Buffer  = True
  writePortModeVHDL Linkage = False

  public export
  writePortMode : PortMode l -> Bool
  writePortMode SVP = False
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


-- namespace CanConnectTypes

--   public export
--   data SamePredefinedEnumeration : PredefinedEnumeration -> PredefinedEnumeration -> Type where
--     SCC : SamePredefinedEnumeration CHARACTER CHARACTER
--     SBB : SamePredefinedEnumeration BIT BIT
--     SBO : SamePredefinedEnumeration BOOLEAN BOOLEAN
--     SSS : SamePredefinedEnumeration SEVERITY_LEVEL SEVERITY_LEVEL

--   public export
--   data CanConnectVHDL : DataType VHDL -> DataType VHDL -> Type where
--     CCInt : CanConnectVHDL (VHD $ Integer') (VHD $ Integer')
--     CCPhy : CanConnectVHDL (VHD $ Physical) (VHD $ Physical)
--     CCRea : CanConnectVHDL (VHD $ Real) (VHD $ Real)
--     CCEn  : SamePredefinedEnumeration e e' -> CanConnectVHDL (VHD $ Enum e) (VHD $ Enum e')

--   public export
--   data CanConnectT : (l : Lang) -> DataType l -> DataType l -> Type where
--     CCSV : CanConnectT SystemVerilog t t'
--     CCVH : CanConnectVHDL t t' -> CanConnectT VHDL t t'

-- namespace Bus

  -- public export
  -- data PFin : (l : Lang) -> (usl : DesignUnitSigsList l) -> (subUs : FinsList usl.length) -> (t : DataType l) -> 
  --             (f : Fin $ totalSubs' usl subUs) -> Type where
  --   MkPFin : (f : Fin $ totalSubs' usl subUs) -> CanConnectT l (subPortType usl subUs f) t -> PFin l usl subUs t f
  
  -- ||| Port fins indexed by type
  -- public export
  -- data PFinsList : (l : Lang) -> (usl : DesignUnitSigsList l) -> (subUs : FinsList usl.length) -> (t : DataType l) -> 
  --                  (f : FinsList $ totalSubs' usl subUs) -> Type where
  --   One  : PFin l usl subUs t f -> PFinsList l usl subUs t [ f ]
  --   More : PFin l usl subUs t f -> PFinsList l usl subUs t fs -> PFinsList l usl subUs t (f::fs)

  -- public export
  -- (++) : FinsList n -> FinsList n -> FinsList n
  -- Nil       ++ ys = ys
  -- (x :: xs) ++ ys = x :: (xs ++ ys)

  -- public export
  -- data BusNotEmpty : (l : Lang) -> (s : DesignUnitSig l) -> (usl : DesignUnitSigsList l) -> (subUs : FinsList usl.length) -> 
  --                    (ssc : FinsList $ totalSubs' usl subUs) -> (ssk : FinsList $ totalSubs' usl subUs) -> Type where
  --   FNE : BusNotEmpty l s usl subUs (x::xs) ssk
  --   SNE : BusNotEmpty l s usl subUs ssc (x::xs)

  -- ||| 6.5.6.3 Port clauses
  -- |||
  -- ||| if a formal signal port is associated with
  -- ||| an actual that is itself a port, then the following restrictions apply depending upon the mode (see 6.5.2), if
  -- ||| any, of the formal signal port:
  -- ||| a) For a formal signal port of mode in the associated actual shall be a port of mode in, out, inout, or
  -- ||| buffer. This restriction applies both to an actual that is associated as a name in the actual part of an
  -- ||| association element and to an actual that is associated as part of an expression in the actual part of an
  -- ||| association element.
  -- ||| b) For a formal signal port of mode out, the associated actual shall be a port of mode out, inout, or
  -- ||| buffer.
  -- ||| c) For a formal signal port of mode inout, the associated actual shall be a port of mode out, inout, or
  -- ||| buffer.
  -- ||| d) For a formal signal port of mode buffer, the associated actual shall be a port of mode out, inout, or
  -- ||| buffer.
  -- ||| e) For a formal signal port of mode linkage, the associated actual may be a port of any mode.
  -- |||
  -- ||| IEEE 1076-2019
  -- |||
  -- ||| So out, inout, buffer and linkage can drive
  -- public export
  -- data IsBusDrivenGood : (l : Lang) -> (usl : DesignUnitSigsList l) -> (subUs : FinsList usl.length) -> 
  --                    (ssc : FinsList $ totalSubs' usl subUs) -> (ssk : FinsList $ totalSubs' usl subUs) -> (t : DataType l) -> Type

  -- public export
  -- data Bus : (l : Lang) -> (usl : DesignUnitSigsList l) -> (subUs : FinsList usl.length) ->
  --            (usedPorts : FinsList $ totalSubs' usl subUs) -> Type where
  --   MkBus : {l : _} -> {usl : _} -> {subUs : _} ->
  --           (t : DataType l) ->
  --           {scfs : _} -> {skfs : _} ->
  --           (ssc : PFinsList l usl subUs t scfs) -> (ssk : PFinsList l usl subUs t skfs) -> -- lists non empty and compatible with t
  --           -- Check if bus is driven correct
  --           -- (0 dr : IsBusDrivenGood l usl subUs scfs skfs t) =>
  --           Bus l usl subUs (scfs ++ skfs)
  
  -- public export
  -- data BusesList : (l : Lang) -> (usl : DesignUnitSigsList l) -> (subUs : FinsList usl.length) ->
  --                  (usedPorts : FinsList $ totalSubs' usl subUs) -> Type where
  --   Nil  : BusesList l usl subUs []
  --   (::) : Bus l usl subUs up -> BusesList l usl subUs up' -> BusesList l usl subUs (up ++ up')
  
  -- public export
  -- length : BusesList l usl subUs up -> Nat
  -- length []      = Z
  -- length (_::usl) = S $ length usl

  -- public export %inline
  -- (.length) : BusesList l usl subUs up -> Nat
  -- (.length) = length

  -- public export
  -- index : (bsl : BusesList l usl subUs up) -> Fin bsl.length -> (up' : FinsList (totalSubs' usl subUs) ** Bus l usl subUs up')
  -- index (bus@(MkBus _ _ _ {scfs} {skfs})::_)   FZ    = ((scfs ++ skfs) ** bus)
  -- index (_                              ::bsl) (FS i) = index bsl i

-- namespace PortConnsTTS

--   public export
--   data TopToSubConnection : (l : Lang) -> (s : DesignUnitSig l) -> (usl : DesignUnitSigsList l) -> (subUs : FinsList usl.length) ->
--                             (preUsedPorts : FinsList $ totalSubs' usl subUs) -> (aftUsedPorts : FinsList $ totalSubs' usl subUs) -> Type where
--     MkTTS : {l : _} -> {s : _} -> {usl : _} -> {subUs : _} -> {preUsedPorts : _} ->
--              (tf : Fin $ totalTops' s) ->
--              (sf : Fin $ totalSubs' usl subUs) ->
--              TopToSubConnection l s usl subUs preUsedPorts (sf::preUsedPorts)
  
--   public export
--   data TTSConnsList : (l : Lang) -> (s : DesignUnitSig l) -> (usl : DesignUnitSigsList l) -> (subUs : FinsList usl.length) ->
--                       (preUsedPorts : FinsList $ totalSubs' usl subUs) -> (aftUsedPorts : FinsList $ totalSubs' usl subUs) -> Type where
--     Nil  : TTSConnsList l s usl subUs pre pre
--     (::) : TopToSubConnection l s usl subUs mid aft -> TTSConnsList l s usl subUs pre mid -> TTSConnsList l s usl subUs pre aft

--   public export
--   length : TTSConnsList l s usl subUs pre aft -> Nat
--   length []      = Z
--   length (_::ps) = S $ length ps

--   public export %inline
--   (.length) : TTSConnsList l s usl subUs pre aft -> Nat
--   (.length) = length

  -- public export
  -- index : (ps : TTSConnsList l s usl subUs pre aft) -> Fin ps.length -> TopToSubConnection l s usl subUs mid aft
  -- index (p::_ ) FZ     = p
  -- index (_::ps) (FS i) = index ps i

-- namespace PortConnsTTB

--   public export
--   data TopToBusConnection : (l : Lang) -> (s : DesignUnitSig l) -> (usl : DesignUnitSigsList l) -> (subUs : FinsList usl.length) ->
--                             (usedPorts : FinsList $ totalSubs' usl subUs) -> {up' : _} -> (buses : BusesList l usl subUs up') -> Type where
--     MkTTB : {l : _} -> {s : _} -> {usl : _} -> {subUs : _} -> {usedPorts : _} -> {up' : _} -> {buses : BusesList l usl subUs up'} ->
--              (tf : Fin $ totalTops' s) ->
--              (bf : Fin buses.length) ->
--              -- check if previous top ports already drive same bus
--              TopToBusConnection l s usl subUs usedPorts buses
    
--   public export
--   data TTBList : (l : Lang) -> (s : DesignUnitSig l) -> (usl : DesignUnitSigsList l) -> (subUs : FinsList usl.length) ->
--                  (usedPorts : FinsList $ totalSubs' usl subUs) -> {up' : _} -> (buses : BusesList l usl subUs up') -> Type where
--     Nil  : TTBList l s usl subUs usedPorts buses
--     (::) : TopToBusConnection l s usl subUs usedPorts buses -> TTBList l s usl subUs usedPorts buses -> TTBList l s usl subUs usedPorts buses

--   public export
--   length : TTBList l s usl subUs usedPorts buses -> Nat
--   length []      = Z
--   length (_::ps) = S $ length ps

--   public export %inline
--   (.length) : TTBList l s usl subUs usedPorts buses -> Nat
--   (.length) = length

-- namespace Link

  -- public export
  -- totalTops : DesignUnitSig l -> PortsList l
  -- totalTops = .ports

  -- public export
  -- totalTops' : DesignUnitSig l -> Nat
  -- totalTops' s = length $ totalTops s

  -- public export
  -- totalSubs : (usl : DesignUnitSigsList l) -> (subUs : FinsList usl.length) -> PortsList l
  -- totalSubs usl []      = []
  -- totalSubs usl (f::fs) = (totalTops $ index usl f) ++ totalSubs usl fs

  -- public export
  -- totalSubs' : (usl : DesignUnitSigsList l) -> (subUs : FinsList usl.length) -> Nat
  -- totalSubs' usl subUs = length $ totalSubs usl subUs

  -- public export
  -- subPort : (usl : DesignUnitSigsList l) -> (subUs : FinsList usl.length) -> Fin (totalSubs' usl subUs) -> Port l
  -- subPort usl subUs = index $ totalSubs usl subUs

  -- public export
  -- typeOfSubPort : (usl : DesignUnitSigsList l) -> (subUs : FinsList usl.length) -> Fin (totalSubs' usl subUs) -> DataType l
  -- typeOfSubPort usl subUs f = (subPort usl subUs f).type

  -- public export
  -- data FinToAnyPort : M

  -- public export
  -- data Wire : (l : Lang) -> (s : DesignUnitSig l) ->
  --             (usl : DesignUnitSigsList l) -> (subUs : FinsList usl.length) -> Type where
  --   MkWire : {l : _} -> {s : _} -> {usl : _} -> {subUs : _} -> 
  --            (topSrc : MFin (totalTops' s)) -> (topSnk : MFin (totalTops' s)) ->
  --            (subSrcs : FinsList $ totalSubs' usl subUs) -> (subSnks : FinsList $ totalSubs' usl subUs) -> Wire l s usl subUs
  
  -- public export
  -- data LinkNotEmpty : (tsc : MFin a) -> (tsk : MFin a) -> (ssc : FinsList b) -> (ssk : FinsList b) -> Type where
  --   JTSK : LinkNotEmpty (Just f) Nothing ssc       ssk
  --   JTSC : LinkNotEmpty Nothing (Just f) ssc       ssk
  --   JSSC : LinkNotEmpty Nothing Nothing  (x :: xs) ssk
  --   JSSK : LinkNotEmpty Nothing Nothing  ssc       (x :: xs)
  

  -- public export
  -- data CanDriveVHDL : VHDLPortMode -> Type
  -- public export
  -- data CanBeDrivenVHDL : VHDLPortMode -> Type

  -- public export
  -- data CanDrive : PortMode l -> Type
  -- public export
  -- data CanBeDriven : PortMode l -> Type

--   public export
--   data IsMultidriven : DataType l -> Type

--   public export
--   data IsSingleDriven : DataType l -> Type

--   public export
--   data Drivers : (l : Lang) -> (s : DesignUnitSig l) ->
--                  (usl : DesignUnitSigsList l) -> (subUs : FinsList usl.length) -> 
--                  (tsc : MFin (totalTops' s)) -> (ssc : FinsList $ totalSubs' usl subUs) -> DataType l -> Type where
--     OneTop  : IsSingleDriven t -> Drivers l s usl subUs (Just f) [] t
--     OneSub  : IsSingleDriven t -> Drivers l s usl subUs Nothing (f::[]) t
--     NoDrive : IsSingleDriven t -> Drivers l s usl subUs Nothing [] t
--     MD      : IsMultidriven t -> Drivers l s usl subUs tsc ssc t

--   ||| If top port is declated, then the whole link is a connection to a port, if only subs, it means it's a bus.
--   public export
--   data Link : (l : Lang) -> (s : DesignUnitSig l) ->
--              (usl : DesignUnitSigsList l) -> (subUs : FinsList usl.length) -> Type where
--     MkLink : {l : _} -> {s : _} -> {usl : _} -> {subUs : _} -> 
--             (tsc : MFin (totalTops' s)) -> (tsk : MFin (totalTops' s)) ->
--             (ssc : FinsList $ totalSubs' usl subUs) -> (ssk : FinsList $ totalSubs' usl subUs) ->
--             -- Only one top port is legal. Otherwise we'd have to give two top ports same id
--             (0 ne : LinkNotEmpty tsc tsk ssc ssk) =>
--             -- Type of the whole Link. Same as top port or dependent on lang's logic
--             (t : DataType l) ->
--             -- If Link is driven legally
--             (0 drivers : Drivers l s usl subUs tsc ssc t) =>
--             Link l s usl subUs
  
--   public export
--   data LinkesList : (l : Lang) -> (s : DesignUnitSig l) ->
--                    (usl : DesignUnitSigsList l) -> (subUs : FinsList usl.length) -> Type where
--     Nil  : LinkesList l s usl subUs
--     (::) : Link l s usl subUs -> LinkesList l s usl subUs -> LinkesList l s usl subUs


-- namespace GenLinkes

--   public export
--   natToFin' : Nat -> (n : Nat) -> MFin n
--   natToFin' i n = case natToFin i n of
--     Nothing  => Nothing
--     (Just x) => Just x

--   public export
--   data JustFin : MFin n -> Fin n -> Type where
--     JF : JustFin (Just x) x

  -- public export
  -- newTop : {l : _} -> {s : _} -> {usl : _} -> {subUs : _} ->
  --          Fin (totalTops' s) -> Link l s usl subUs
  -- newTop f = MkLink (Just f) []

  -- public export
  -- newSub : {l : _} -> {s : _} -> {usl : _} -> {subUs : _} ->
  --          Fin (totalSubs' usl subUs) -> Link l s usl subUs
  -- newSub f = MkMC Nothing [ f ]

  -- public export
  -- addSub : Fin (totalSubs' usl subUs) -> MultiConnection l s usl subUs ->  MultiConnection l s usl subUs
  -- addSub f (MkMC top subs) = MkMC top (f :: subs)

  -- public export
  -- insertAt0 : (pre : LinkesList l s usl subUs) -> Link l s usl subUs -> LinkesList l s usl subUs
  -- insertAt0 pre mc = mc :: pre

  -- public export
  -- replaceAt : (pre : MultiConnectionsList l s usl subUs) -> Fin (length pre) -> MultiConnection l s usl subUs -> MultiConnectionsList l s usl subUs
  -- replaceAt []        FZ     _ impossible
  -- replaceAt []        (FS _) _ impossible
  -- replaceAt (_ :: xs) FZ     mc = mc :: xs
  -- replaceAt (x :: xs) (FS i) mc = x :: replaceAt xs i mc

  -- public export
  -- isMultidriven : MultiConnection l s usl subUs -> Bool

  -- public export
  -- isDriver : Port l -> Bool

  -- public export
  -- arePortTypesCompatible : DataType l -> DataType l -> Bool

  -- public export
  -- isCompatible : MultiConnection l s usl subUs -> Port l -> Bool
  -- isCompatible mc p with (isMultidriven mc && isDriver p)
  --   isCompatible mc p | b = ?hbjn

  -- public export
  -- compatibleMCSs : MultiConnectionsList l s usl subUs -> Port l -> MultiConnectionsList l s usl subUs
  -- compatibleMCSs []          _ = []
  -- compatibleMCSs (mc :: mcs) p = case isCompatible mc p of
  --   False => compatibleMCSs mcs p
  --   True  => mc :: compatibleMCSs mcs p

  -- public export
  -- data FillTop : (l : Lang) -> (s : DesignUnitSig l) ->
  --                (usl : DesignUnitSigsList l) -> (subUs : FinsList usl.length) ->
  --                LinkesList l s usl subUs -> (topi : Nat) -> LinkesList l s usl subUs -> Type where
  --   TEnd      : FillTop l s usl subUs pre 0 pre
  --   TNew      : (recur : FillTop l s usl subUs pre k mid) ->
  --               {jf : JustFin (natToFin' k $ totalTops' s) topF} ->
  --               FillTop l s usl subUs pre (S k) $ insertAt0 mid   $ newTop topF
  
  -- public export
  -- data FillSub : (l : Lang) -> (s : DesignUnitSig l) ->
  --                (usl : DesignUnitSigsList l) -> (subUs : FinsList usl.length) ->
  --                LinkesList l s usl subUs -> (subi : Nat) -> LinkesList l s usl subUs -> Type where
  --   SEnd      : FillSub l s usl subUs pre 0 pre
  --   SNew      : (recur : FillSub l s usl subUs pre k mid) ->
  --               {jf : JustFin (natToFin' k $ totalSubs' usl subUs) subF} -> 
  --               FillSub l s usl subUs pre (S k) $ insertAt0 mid   $ newSub subF
  --   SExisting : (recur : FillSub l s usl subUs pre k mid) ->
  --               (f : Fin $ length mid) -> {jf : JustFin (natToFin' k $ totalSubs' usl subUs) subF} ->
  --               (cc : CanConnect l (typeOf $ index mid f)  (typeOfSubPort usl subUs subF)) ->
  --               FillSub l s usl subUs pre (S k) $ replaceAt mid f $ addSub subF $ index mid f
  --               -- `index mid f` is where we add port
  --               -- subF is which port we add

--   public export
--   data GenLinkes : (l : Lang) -> (s : DesignUnitSig l) ->
--                        (usl : DesignUnitSigsList l) -> (subUs : FinsList usl.length) -> 
--                        LinkesList l s usl subUs -> Type where
--     GenMC : (ft : FillTop l s usl subUs []        (totalTops' s)         filledTop) -> 
--             (fs : FillSub l s usl subUs filledTop (totalSubs' usl subUs) filledSub) ->
--             GenLinkes l s usl subUs filledSub

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
           -- For VHDL sub source (out, inout, buffer) can be added only if no top inputs
  
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
  fromList : List (MultiConnection l s usl subUs) -> MultiConnectionsList l s usl subUs
  fromList []        = []
  fromList (x :: xs) = x :: fromList xs

  public export
  typeOf : {l : _} -> {s : _} -> {usl : _} -> {subUs : _} ->
          MultiConnection l s usl subUs -> DataType l
  typeOf (MkMC (Just x) tsk ssc ssk  {ne = JustTSC}) = (index (totalTops s) x).type
  typeOf (MkMC tsc (Just x) ssc ssk  {ne = JustTSK}) = (index (totalTops s) x).type
  typeOf (MkMC tsc tsk (x :: xs) ssk {ne = JustSSC}) = (index (totalSubs usl subUs) x).type
  typeOf (MkMC tsc tsk ssc (x :: xs) {ne = JustSSK}) = (index (totalSubs usl subUs) x).type

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

-- namespace CAP

  public export
  isResolved : DataType l -> Bool
  isResolved (SVT _)        = True -- TODO: make uwire net unresolved
  isResolved (VHD StdLogic) = True
  isResolved (VHD _)        = False

--   -- isTopSink : {s : _} -> MFin (totalTops' s) -> Bool
--   -- isTopSink Nothing  = False
--   -- isTopSink (Just f) = finToNat f < s.inpsCount

--   -- subsOnlySinks : (subs : FinsList $ totalSubs' usl subUs) -> Bool
--   -- subsOnlySinks []        = True
--   -- subsOnlySinks (f :: fs) = False || subsOnlySinks fs

--   -- public export
--   -- noSource : {s : _} -> {usl : _} -> {subUs : _} -> MultiConnection l s usl subUs -> Bool
--   -- noSource (MkMC top subs) = isTopSink top && subsOnlySinks subs

  -- public export
  -- data CanDrive : {l : _} -> {s : _} -> {usl : _} -> {subUs : _} ->
  --                 MultiConnection l s usl subUs -> Fin (totalSubs' usl subUs) -> Type where
  --   JustSink : So (not $ isSubSource {usl} {subUs} f) => CanDrive {usl} {subUs} mc f
  --   -- NoSourceHasTop : CanDrive (MkMC Nothing (Just x) [] ssk) f
  --   NoSource : (ne : MCNotEmpty Nothing tsk [] ssk) => CanDrive {l} {s} {usl} {subUs} (MkMC {l} {s} {usl} {subUs} Nothing tsk  [] ssk @{ne}) f
  --   IsMultidriven  : So (isResolved $ typeOf mc) => CanDrive mc f

  public export
  canDrive : {l : _} -> {s : _} -> {usl : _} -> {subUs : _} ->
             MultiConnection l s usl subUs -> Fin (totalSubs' usl subUs) -> Bool
  canDrive mc f with ((not $ isSubSource {usl} {subUs} f) || (isResolved $ typeOf mc)) -- f is sink or mc is multidriven
    canDrive (MkMC Nothing tsk []  ssk) f | False = True
    canDrive (MkMC tsc     tsk ssc ssk) f | False = False
    canDrive mc                         f | True  = True
  -- canDrive (MkMC tsc tsk ssc ssk {ne}) f = ?jklm_0    


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

  -- public export
  -- isMultidriven : MultiConnection l s usl subUs -> Bool

  -- public export
  -- isDriver : Port l -> Bool

  -- public export
  -- arePortTypesCompatible : DataType l -> DataType l -> Bool

  -- public export
  -- isCompatible : MultiConnection l s usl subUs -> Port l -> Bool
  -- isCompatible mc p with (isMultidriven mc && isDriver p)
  --   isCompatible mc p | b = ?hbjn

  -- public export
  -- compatibleMCSs : MultiConnectionsList l s usl subUs -> Port l -> MultiConnectionsList l s usl subUs
  -- compatibleMCSs []          _ = []
  -- compatibleMCSs (mc :: mcs) p = case isCompatible mc p of
  --   False => compatibleMCSs mcs p
  --   True  => mc :: compatibleMCSs mcs p


  public export
  checkLinkage : VHDLPortMode -> VHDLPortMode -> Bool
  checkLinkage In b = True
  checkLinkage Out b = True
  checkLinkage InOut b = True
  checkLinkage Buffer b = True
  checkLinkage Linkage In = False
  checkLinkage Linkage Out = False
  checkLinkage Linkage InOut = False
  checkLinkage Linkage Buffer = False
  checkLinkage Linkage Linkage = True
  
  public export
  checkPortModes : {l : _} -> PortMode l -> PortMode l -> Bool
  checkPortModes {l = SystemVerilog} pm pm' = True
  checkPortModes {l = VHDL} (VHP pm) (VHP pm') = checkLinkage pm pm'

  -- data 

  -- public export
  -- data CheckPortModes : {l : _} -> PortMode l -> PortMode l -> Type where
  --   SVCPM : CheckPortModes SystemVerilog pm pm'
  --   VHCPM : CheckPortModes VHDL pm pm'

  -- public export
  -- data PortModesCompatible : {l : _} -> {s : _} -> {usl : _} -> {subUs : _} ->
  --                            MultiConnection l s usl subUs -> Fin (totalSubs' usl subUs) -> Type where
  --   TSKCompatible : (ne : MCNotEmpty Nothing (Just tf) ssc ssk) => So (checkPortModes (topPortMode s tf) (subPortMode usl subUs f) ) => 
  --                   PortModesCompatible {l} {s} {usl} {subUs} (MkMC {l} {s} {usl} {subUs} Nothing (Just tf) ssc ssk @{ne}) f
  --   TSCCompatible : (ne : MCNotEmpty (Just tf) Nothing ssc ssk) => So (checkPortModes (topPortMode s tf) (subPortMode usl subUs f) ) => 
  --                   PortModesCompatible {l} {s} {usl} {subUs} (MkMC {l} {s} {usl} {subUs} (Just tf) Nothing ssc ssk @{ne}) f
  --   NoTopSC       : (ne : MCNotEmpty Nothing Nothing ssc ssk) => 
  --                   PortModesCompatible {l} {s} {usl} {subUs} (MkMC {l} {s} {usl} {subUs} Nothing Nothing ssc ssk @{ne}) f

  public export
  portModesCompatible : {l : _} -> {s : _} -> {usl : _} -> {subUs : _} ->
                        MultiConnection l s usl subUs -> Fin (totalSubs' usl subUs) -> Bool
  portModesCompatible (MkMC (Just x) Nothing ssc ssk @{ne} @{OnlyTSC}) f = checkPortModes (topPortMode s x) (subPortMode usl subUs f)  
  portModesCompatible (MkMC Nothing (Just x) ssc ssk @{ne} @{OnlyTSK}) f = checkPortModes (topPortMode s x) (subPortMode usl subUs f)  
  portModesCompatible (MkMC Nothing Nothing  ssc ssk @{ne} @{NoTop})   f = True   

  public export
  buildTopMCS : (l : Lang) -> (s : DesignUnitSig l) ->
                (usl : DesignUnitSigsList l) -> (subUs : FinsList usl.length) -> MultiConnectionsList l s usl subUs
  buildTopMCS _ s _ _ = fromList $ map newTop $ List.allFins $ totalTops' s

  -- public export
  -- data FillTop : (l : Lang) -> (s : DesignUnitSig l) ->
  --                (usl : DesignUnitSigsList l) -> (subUs : FinsList usl.length) ->
  --                MultiConnectionsList l s usl subUs -> (topi : Nat) -> MultiConnectionsList l s usl subUs -> Type where
  --   TEnd : FillTop l s usl subUs pre 0 pre
  --   TNew : (recur : FillTop l s usl subUs pre k mid) ->
  --         --  {jf : JustFin (natToFin' k $ totalTops' s) topF} ->
  --          FillTop l s usl subUs pre (S k) $ newTop' k :: mid

  public export
  isGoodMC : {l : _} -> {s : _} -> {usl : _} -> {subUs : _} ->
             MultiConnection l s usl subUs -> Fin (totalSubs' usl subUs) -> Bool
  isGoodMC mc subF = (typeOf mc) == (subPortType usl subUs subF)
                  && canDrive mc subF
                  && portModesCompatible mc subF

  public export
  filterGoodMCs : {l : _} -> {s : _} -> {usl : _} -> {subUs : _} ->
                  (mid : MultiConnectionsList l s usl subUs) -> List (Fin (length mid)) -> Fin (totalSubs' usl subUs) -> FinsList (length mid)
  filterGoodMCs mid []      subF = []
  filterGoodMCs mid (f::fs) subF = case isGoodMC (index mid f) subF of
    False => filterGoodMCs mid fs subF
    True  => f :: filterGoodMCs mid fs subF

  public export
  filterGoodMCs' : {l : _} -> {s : _} -> {usl : _} -> {subUs : _} ->
                   (mid : MultiConnectionsList l s usl subUs) -> List (Fin (length mid)) -> Nat -> FinsList (length mid)
  filterGoodMCs' mid fins k = case natToFin' k $ totalSubs' usl subUs of
    Nothing     => [] -- impossible
    (Just subF) => filterGoodMCs mid fins subF
  
  public export
  goodFins : {l : _} -> {s : _} -> {usl : _} -> {subUs : _} ->
             (mid : MultiConnectionsList l s usl subUs) -> Nat -> FinsList (length mid)
  goodFins mid k = filterGoodMCs' mid (List.allFins $ length mid) k
  
  public export
  data FillSub : (l : Lang) -> (s : DesignUnitSig l) ->
                 (usl : DesignUnitSigsList l) -> (subUs : FinsList usl.length) ->
                 MultiConnectionsList l s usl subUs -> (subi : Nat) -> MultiConnectionsList l s usl subUs -> Type where
    SEnd      : FillSub l s usl subUs pre 0 pre
    SNew      : (recur : FillSub l s usl subUs pre k mid) ->
                -- {jf : JustFin (natToFin' k $ totalSubs' usl subUs) subF} -> 
                FillSub l s usl subUs pre (S k) $ newSub' k :: mid
    SExisting : (recur : FillSub l s usl subUs pre k mid) ->
                (f : Fin (goodFins mid k).length) -> 
                FillSub l s usl subUs pre (S k) 
                  $ replaceAt mid (index (goodFins mid k) f) 
                  $ addSub' k 
                  $ index mid (index (goodFins mid k) f)
                -- k (subF) is which port we add
                -- `index mid f` is where we add
                -- 3 preds VS construct VS workaround (fin to lookup)

  public export
  data GenMulticonns : (l : Lang) -> (s : DesignUnitSig l) ->
                       (usl : DesignUnitSigsList l) -> (subUs : FinsList usl.length) -> 
                       MultiConnectionsList l s usl subUs -> Type where
    GenMC : -- (ft : FillTop l s usl subUs []        (totalTops' s)         filledTop) -> 
            (fs : FillSub l s usl subUs (buildTopMCS l s usl subUs) (totalSubs' usl subUs) filledSub) -> -- (buildTopMCS l s usl subUs)
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
