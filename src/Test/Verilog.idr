module Test.Verilog

import Data.Fuel
import Data.Vect
import public Data.Fin

import Test.DepTyCheck.Gen

%default total

namespace SVTypes

  ||| Variable types
  |||
  ||| |  Type     | Description                                                     |
  ||| |-----------|-----------------------------------------------------------------|
  ||| | shortint  | 2-state data type, 16-bit signed integer                        |
  ||| | int       | 2-state data type, 32-bit signed integer                        |
  ||| | longint   | 2-state data type, 64-bit signed integer                        |
  ||| | byte      | 2-state data type, 8-bit signed integer or ASCII character      |
  ||| | bit       | 2-state data type, user-defined vector size, unsigned           |
  ||| | logic     | 4-state data type, user-defined vector size, unsigned           |
  ||| | reg       | 4-state data type, user-defined vector size, unsigned           |
  ||| | integer   | 4-state data type, 32-bit signed integer                        |
  ||| | time      | 4-state data type, 64-bit unsigned integer                      |
  ||| | real      | The “real” data type is 64-bit                                  |
  ||| | shortreal | The “shortreal” data type is 32-bit                             |
  ||| | realtime  | The “realtime” declarations is treated synonymously with “real” |
  |||
  ||| Net types
  |||
  ||| | Net     | Description                                             |
  ||| |---------|---------------------------------------------------------|
  ||| | wire    | A high impedance net; multi-driver net                  |
  ||| | tri     | A high impedance net; multi-driver net                  |
  ||| | tri0    | Resistive pulldown net                                  |
  ||| | tri1    | Resistive pullup net                                    |
  ||| | trior   | Same as “wor”; “1” wins in all cases; multi-driver net  |
  ||| | triand  | Same as “wand”; “0” wins in all cases; multi-driver net |
  ||| | trireg  | Models charge storage node                              |
  ||| | uwire   | Unresolved type; allows only one driver on the net      |
  ||| | wand    | Same as “triand”; “0” wins in all cases                 |
  ||| | wor     | Same as trior; “1” wins in all cases                    |
  ||| | supply0 | Net with supply strength to model “gnd”                 |
  ||| | supply1 | Net with supply strength to model “power”               |
  |||
  ||| Ashok B. Mehta. Introduction to SystemVerilog, 2021
  public export
  data SVType = Logic' | Wire' | Uwire' | Int' | Integer' | Bit' | Real'

  public export
  data EqSVType : SVType -> SVType -> Type where
    EqLogic'   : EqSVType Logic'   Logic'
    EqWire'    : EqSVType Wire'    Wire'
    EqUwire'   : EqSVType Uwire'   Uwire'
    EqInt'     : EqSVType Int'     Int'
    EqInteger' : EqSVType Integer' Integer'
    EqBit'     : EqSVType Bit'     Bit'
    EqReal'    : EqSVType Real'    Real'

  ||| 7.4.1 Packed arrays
  ||| Packed arrays can be made of only the single bit data types (bit, logic, reg)
  |||
  ||| IEEE 1800-2023
  public export
  data AllowedInPackedArr : SVType -> Type where
    B : AllowedInPackedArr Bit'
    L : AllowedInPackedArr Logic'
    -- R : AllowedInPackedArr Reg' -- Uncomment when Reg is added to the SVType

  ||| The main difference between an unpacked array and a packed array is that
  ||| an unpacked array is not guaranteed to be represented as a contiguous set of bits
  public export
  data SVArray : SVType -> Nat -> Nat -> Type where
    Unpacked   : (t : SVType) -> (start : Nat) -> (end : Nat) -> SVArray t start end
    Packed     : (t : SVType) -> (start : Nat) -> (end : Nat) -> AllowedInPackedArr t -> SVArray t start end

namespace Ports
  public export
  data PortType = Arr (SVArray _ _ _) | Var SVType

  public export
  data PortsList = Nil | (::) PortType PortsList

  public export
  length : PortsList -> Nat
  length []      = Z
  length (_::ms) = S $ length ms

  public export %inline
  (.length) : PortsList -> Nat
  (.length) = length

  public export
  (++) : PortsList -> PortsList -> PortsList
  Nil       ++ ys = ys
  (x :: xs) ++ ys = x :: (xs ++ ys)

  public export
  toList : PortsList -> List PortType
  toList []        = []
  toList (x :: xs) = x :: toList xs

  export
  portsListAppendLen : (xs : PortsList) -> (ys : PortsList) -> length xs + length ys = length (xs ++ ys)
  portsListAppendLen []        ys = Refl
  portsListAppendLen (_ :: xs) ys = rewrite portsListAppendLen xs ys in Refl

  -- Maybe, specialised type `IndexIn : PortsList -> Type` isomorphic to `Fin (length xs)`

  public export
  typeOf : (xs : PortsList) -> Fin (length xs) -> PortType
  typeOf (p::_ ) FZ     = p
  typeOf (_::ps) (FS i) = typeOf ps i

namespace ModuleSig

  public export
  record ModuleSig where
    constructor MkModuleSig
    inputs  : PortsList
    outputs : PortsList

  public export
  (.inpsCount) : ModuleSig -> Nat
  (.inpsCount) m = length m.inputs

  public export
  (.outsCount) : ModuleSig -> Nat
  (.outsCount) m = length m.outputs

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
allInputs : {ms : ModuleSigsList} -> FinsList ms.length -> PortsList
allInputs []      = []
allInputs (i::is) = (index ms i).inputs ++ allInputs is

public export
allOutputs : {ms : ModuleSigsList} -> FinsList ms.length -> PortsList
allOutputs []      = []
allOutputs (i::is) = (index ms i).outputs ++ allOutputs is

namespace ConnectionsValidation
  public export
  data EqNat : Nat -> Nat -> Type where
    Same : (x : Nat) -> EqNat x x

  public export
  data VarOrPacked : PortType -> Type where
    V : VarOrPacked (Var _)
    P : VarOrPacked (Arr (Packed _ _ _ _))

  public export
  data CanConnect : PortType -> PortType -> Type where
    CCVarOrPacked : VarOrPacked p1 -> VarOrPacked p2 -> CanConnect p1 p2
    ||| 6.22.2 Equivalent types
    ||| d) Unpacked fixed-size array types are equivalent if they have equivalent element types and equal size.
    ||| IEEE 1800 - 2023
    CCUnpackedUnpacked : EqSVType t t' -> EqNat (plus (max s e) (min s' e')) (plus (max s' e') (min s e))  ->
      CanConnect (Arr (Unpacked t s e)) (Arr (Unpacked t' s' e'))

  ||| The list of sources may be empty (Nil). In this case, either an implicit net is declared or an external net declaration must exist
  |||
  ||| > If an identifier is used in a port expression declaration,
  ||| then an implicit net of default net type shall be assumed, with the vector width of the port expression declaration.
  ||| IEEE 1800-2023
  public export
  data SourceForSink : (srcs : PortsList) -> (sink : PortType) -> Type where
      NoSource     : SourceForSink srcs sink
      SingleSource : (srcIdx : Fin $ length srcs) -> CanConnect (typeOf srcs srcIdx) sink -> SourceForSink srcs sink

namespace ConnsList
  ||| Each output has a connection from some single input.
  ||| Each input can go to several outputs.
  public export
  data Connections : (srcs, sinks : PortsList) -> Type where
    Nil  : Connections srcs []
    (::) : SourceForSink srcs sink -> Connections srcs sinks -> Connections srcs (sink :: sinks)

public export
data Modules : ModuleSigsList -> Type where

  End : Modules ms

  ||| A module containing submodules and connections.
  NewCompositeModule :
    (m : ModuleSig) ->
    (subMs : FinsList ms.length) ->
    (sssi : Connections (m.inputs ++ allOutputs {ms} subMs) (allInputs {ms} subMs ++ m.outputs)) ->
    (cont : Modules (m::ms)) ->
    Modules ms

export
genModules : Fuel -> (ms : ModuleSigsList) -> Gen MaybeEmpty $ Modules ms
