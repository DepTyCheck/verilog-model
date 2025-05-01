module Test.Verilog.SVType

import Data.Fuel
import Data.Vect

import public Test.Common.Utils

%default total

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
data SVBasic = Logic' | Wire' | Uwire' | Int' | Integer' | Bit' | Real'

public export
data EqSVBasic : SVBasic -> SVBasic -> Type where
  EqLogic'   : EqSVBasic Logic'   Logic'
  EqWire'    : EqSVBasic Wire'    Wire'
  EqUwire'   : EqSVBasic Uwire'   Uwire'
  EqInt'     : EqSVBasic Int'     Int'
  EqInteger' : EqSVBasic Integer' Integer'
  EqBit'     : EqSVBasic Bit'     Bit'
  EqReal'    : EqSVBasic Real'    Real'

data SVType : Type
data SVArray : SVType -> Nat -> Nat -> Type
data AllowedInPackedArr : SVType -> Type

public export
data SVType = Arr (SVArray t s e) | Var SVBasic

public export
defaultNetType : SVType
defaultNetType = Var Wire'

||| The main difference between an unpacked array and a packed array is that
||| an unpacked array is not guaranteed to be represented as a contiguous set of bits
|||
||| Ashok B. Mehta. Introduction to SystemVerilog, 2021
|||
||| 7.4.1
||| Each packed dimension in a packed array declaration shall be specified by a range specification of the form
||| [ constant_expression : constant_expression ]. Each constant expression may be any integer value --
||| positive, negative, or zero, with no unknown (x) or high-impedance (z) bits. The first value may be greater
||| than, equal to, or less than the second value.
|||
||| IEEE 1800-2023
public export
data SVArray : SVType -> Nat -> Nat -> Type where
  Unpacked   : (t : SVType) -> (start : Nat) -> (end : Nat) -> SVArray t start end
  Packed     : (t : SVType) -> (start : Nat) -> (end : Nat) -> AllowedInPackedArr t => SVArray t start end

||| 7.4.1 Packed arrays
||| Packed arrays can be made of only the single bit data types (bit, logic, reg), enumerated types, and
||| recursively other packed arrays and packed structures.
|||
||| IEEE 1800-2023
public export
data AllowedInPackedArr : SVType -> Type where
  B : AllowedInPackedArr (Var Bit')
  L : AllowedInPackedArr (Var Logic')
  -- R : AllowedInPackedArr Reg' -- Uncomment when Reg is added to the SVBasic
  P : AllowedInPackedArr (Arr (Packed {} @{_}))

namespace Ports

  public export
  data PortsList = Nil | (::) SVType PortsList

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
  toList : PortsList -> List SVType
  toList []        = []
  toList (x :: xs) = x :: toList xs

  export
  portsListAppendLen : (xs : PortsList) -> (ys : PortsList) -> length xs + length ys = length (xs ++ ys)
  portsListAppendLen []        ys = Refl
  portsListAppendLen (_ :: xs) ys = rewrite portsListAppendLen xs ys in Refl

  ||| rewrite length from separated to concatenated
  export
  comPS : {0 a, b: PortsList} -> (0 m : Nat -> Type) -> m (length a + length b) -> m (length (a ++ b))
  comPS _ v = rewrite sym $ portsListAppendLen a b in v

  export
  comLen : {0 a, b: PortsList} -> Vect (length a + length b) c -> Vect (length (a ++ b)) c
  comLen = comPS $ \n => Vect n c

  export
  comFin : {0 a, b: PortsList} -> Fin (length a + length b) -> Fin (length (a ++ b))
  comFin = comPS Fin

  -- Maybe, specialised type `IndexIn : PortsList -> Type` isomorphic to `Fin (length xs)`

  public export
  typeOf : (xs : PortsList) -> Fin (length xs) -> SVType
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

public export
allInputs : {ms : ModuleSigsList} -> FinsList ms.length -> PortsList
allInputs []      = []
allInputs (i::is) = (index ms i).inputs ++ allInputs is

public export
allOutputs : {ms : ModuleSigsList} -> FinsList ms.length -> PortsList
allOutputs []      = []
allOutputs (i::is) = (index ms i).outputs ++ allOutputs is

public export
data VarOrPacked : SVType -> Type where
  VPV : VarOrPacked (Var _)
  VPP : VarOrPacked (Arr (Packed {} @{_}))
