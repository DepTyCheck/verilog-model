module Test.Verilog.SVType

import Data.Fuel
import Data.Vect

import public Test.Common.Utils

%default total

||| 6.6.1 Wire and tri nets
||| The wire and tri nets connect elements. The net types wire and tri shall be identical in their syntax and functions;
|||
||| TODO: Print wire or tri randomly
|||
||| 6.6.2 Unresolved nets
||| The uwire net is an unresolved or unidriver wire and is used to model nets that allow only a single driver.
|||
||| 6.6.3 Wired nets
||| Wired nets are of type wor, wand, trior, and triand and are used to model wired logic configurations.
||| The wor and trior nets shall create wired or configurations so that when any of the drivers is 1, the
||| resulting value of the net is 1. The wand and triand nets shall create wired and configurations so that if any
||| driver is 0, the value of the net is 0.
|||
||| 6.6.4 Trireg net
||| The trireg net stores a value and is used to model charge storage nodes. A trireg net can be in one of two
||| states:
||| 1. Driven state - When at least one driver of a trireg net has a value of 1, 0, or x, the resolved
|||                   value propagates into the trireg net and is the driven value of the trireg net.
||| 2. Capacitive state - When all the drivers of a trireg net are at the high-impedance value (z), the
|||                       trireg net retains its last driven value; the high-impedance value does not
|||                       propagate from the driver to the trireg.
|||
||| 6.6.5 Tri0 and tri1 nets
||| The tri0 and tri1 nets model nets with resistive pulldown and resistive pullup devices on them. A tri0
||| net is equivalent to a wire net with a continuous 0 value of pull strength driving it. A tri1 net is equivalent
||| to a wire net with a continuous 1 value of pull strength driving it.
|||
||| 6.6.6 Supply nets
||| The supply0 and supply1 nets can be used to model the power supplies in a circuit. These nets shall have
||| supply strengths.
public export
data NetType = Supply0' | Supply1' | Triand' | Trior' | Trireg' | Tri0' | Tri1' | Uwire' | Wire' | Wand' | Wor'; -- Tri

||| 6.9 Vector declarations
||| A data object declared as reg, logic, or bit (or as a matching user-defined type or implicitly as logic)
||| without a range specification shall be considered 1-bit wide and is known as a scalar. A multibit data object
||| of one of these types shall be declared by specifying a range and is known as a vector. Vectors are packed
||| arrays of scalars
public export
data IntegerVectorType = Byte' | Shortint' | Int' | Longint' | Integer' | Time';

public export
Eq IntegerVectorType where
 (==) Byte'     Byte'     = True
 (==) Shortint' Shortint' = True
 (==) Int'      Int'      = True
 (==) Longint'  Longint'  = True
 (==) Integer'  Integer'  = True
 (==) Time'     Time'     = True
 (==) _         _         = False

public export
data IntegerScalarType = Bit' | Logic' | Reg';

public export
Eq IntegerScalarType where
 (==) Bit'   Bit'   = True
 (==) Logic' Logic' = True
 (==) Reg'   Reg'   = True
 (==) _      _      = False

public export
data NonIntegerType = Shortreal' | Real' | Realtime';

public export
Eq NonIntegerType where
 (==) Shortreal' Shortreal' = True
 (==) Real'      Real'      = True
 (==) _          _          = False

public export
data SVType : Type
public export
data AllowedNetData : SVType -> Type
public export
data PABasic : SVType -> Type

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
|||
||| 6.2 Data types and data objects
|||
||| SystemVerilog makes a distinction between an object and its data type. A data type is a set of values and a
||| set of operations that can be performed on those values. Data types can be used to declare data objects or to
||| define user-defined data types that are constructed from other data types. A data object is a named entity that
||| has a data value and a data type associated with it, such as a parameter, a variable, or a net.
|||
||| 6.4 Singular and aggregate types
|||
||| Data types are categorized as either singular or aggregate. A singular type shall be any data type except an
||| unpacked structure, unpacked union, or unpacked array (see 7.4 on arrays). An aggregate type shall be any
||| unpacked structure, unpacked union, or unpacked array data type. A singular variable or expression
||| represents a single value, symbol, or handle. Aggregate expressions and variables represent a set or
||| collection of singular values. Integral types (see 6.11.1) are always singular even though they can be sliced
||| into multiple singular values. The string data type is singular even though a string can be indexed in a
||| similar way to an unpacked array of bytes.
|||
||| Syntax 6-3—Syntax for variable declarations
|||
||| data_declaration ::=
|||   [ const ] [ var ] [ lifetime ] data_type_or_implicit list_of_variable_decl_assignments ;
|||   | ...
||| data_type_or_implicit ::=
|||   data_type
|||   | implicit_data_type
||| data_type ::=
|||   integer_vector_type [ signing ] { packed_dimension }
|||   | integer_atom_type [ signing ]
|||   | non_integer_type
|||   | struct_union [ packed [ signing ] ] { struct_union_member { struct_union_member } }
|||     { packed_dimension }
|||   | enum [ enum_base_type ] { enum_name_declaration { , enum_name_declaration } }
|||     { packed_dimension }
|||   | string
|||   | chandle
|||   | virtual [ interface ] interface_identifier [ parameter_value_assignment ] [ . modport_identifier ]
|||   | [ class_scope | package_scope ] type_identifier { packed_dimension }
|||   | class_type
|||   | event
|||   | ps_covergroup_identifier
|||   | type_reference18
||| integer_atom_type ::= byte | shortint | int | longint | integer | time
||| integer_vector_type ::= bit | logic | reg
||| non_integer_type ::= shortreal | real | realtime
||| signing ::= signed | unsigned
||| implicit_data_type ::= [ signing ] { packed_dimension }
||| variable_decl_assignment ::=
|||   variable_identifier { variable_dimension } [ = expression ]
|||   | dynamic_array_variable_identifier unsized_dimension { variable_dimension }
|||     [ = dynamic_array_new ]
|||   | class_variable_identifier [ = class_new ]
public export
data SVType : Type where
  -- Implicit : SVType -- Declare an implicit net type
  RVar : NonIntegerType -> SVType
  SVar : IntegerScalarType -> SVType
  VVar : IntegerVectorType -> SVType
  PackedArr : (t : SVType) -> {p : PABasic t} -> Nat -> Nat -> SVType
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
  UnpackedArr : SVType -> Nat -> Nat -> SVType
  -- Dynamic array
  -- Associative array
  -- Queue
  -- Packed Structure
  -- Unpacked Structure
  -- Union
  -- String
  -- Chandle
  -- Enum
  -- Constant (parameter)
  -- Class
  -- Void (?)
  -- Event
  -- User-defined


public export
data SVObject : Type where
  ||| Syntax 6-2—Syntax for net declarations
  |||
  ||| net_declaration16 ::=
  |||   net_type [ drive_strength | charge_strength ] [ vectored | scalared ]
  |||     data_type_or_implicit [ delay3 ] list_of_net_decl_assignments ;
  |||   | nettype_identifier [ delay_control ] list_of_net_decl_assignments ;
  |||   | interconnect implicit_data_type [ # delay_value ]
  |||     net_identifier { unpacked_dimension } [ , net_identifier { unpacked_dimension } ] ;
  ||| net_type ::=
  |||   supply0 | supply1 | tri | triand | trior | trireg | tri0 | tri1 | uwire | wire | wand | wor
  ||| drive_strength ::=
  |||   ( strength0 , strength1 )
  |||   | ( strength1 , strength0 )
  |||   | ( strength0 , highz1 )
  |||   | ( strength1 , highz0 )
  |||   | ( highz0 , strength1 )
  |||   | ( highz1 , strength0 )
  ||| strength0 ::= supply0 | strong0 | pull0 | weak0
  ||| strength1 ::= supply1 | strong1 | pull1 | weak1
  ||| charge_strength ::= ( small ) | ( medium ) | ( large )
  ||| delay3 ::=
  |||   # delay_value
  |||   | # ( mintypmax_expression [ , mintypmax_expression [ , mintypmax_expression ] ] )
  ||| delay_value ::=
  ||| unsigned_number
  |||   | real_number
  |||   | ps_identifier
  |||   | time_literal
  |||   | 1step
  Net : NetType -> (t : SVType) -> {ald : AllowedNetData t} -> SVObject
  Var : SVType -> SVObject

public export
valueOf : SVObject -> SVType
valueOf (Net _ t) = t
valueOf (Var   t) = t

||| 6.6.2 Unresolved nets
||| The uwire net is an unresolved or unidriver wire and is used to model nets that allow only a single driver.
data ResolvedNet : SVObject -> Type where
  NS0 : ResolvedNet $ Net Supply0' v
  NS1 : ResolvedNet $ Net Supply1' v
  NTD : ResolvedNet $ Net Triand' v
  NTR : ResolvedNet $ Net Trior' v
  NTG : ResolvedNet $ Net Trireg' v
  NT0 : ResolvedNet $ Net Tri0' v
  NT1 : ResolvedNet $ Net Tri1' v
  NWI : ResolvedNet $ Net Wire' v
  NWA : ResolvedNet $ Net Wand' v
  NWO : ResolvedNet $ Net Wor' v

||| 7.4.1 Packed arrays
||| Packed arrays can be made of only the single bit data types (bit, logic, reg), enumerated types, and
||| recursively other packed arrays and packed structures.
public export
data PABasic : SVType -> Type where
  PS : PABasic $ SVar s
  PA : PABasic $ PackedArr p s e

||| 6.11.1 Integral types
|||
||| The term integral is used throughout this standard to refer to the data types that can represent a single basic
||| integer data type, packed array, packed structure, packed union, or enum.
public export
data SVIntegral : SVType -> Type where
  ST : SVIntegral $ SVar t
  VT : SVIntegral $ VVar t
  PT : {t : SVType} -> {p : PABasic t} -> SVIntegral $ PackedArr t {p} s e
  -- Packed struct, union, enum

public export
data State4S : IntegerScalarType -> Type where
  S4L : State4S Logic'
  S4R : State4S Reg'

public export
data State4V : IntegerVectorType -> Type where
  V4I : State4V Integer'
  V4T : State4V Time'

public export
data State4 : SVIntegral svt -> Type where
  SS : State4S t => State4 $ ST {t}
  SV : State4V t => State4 $ VT {t}
  SP : {t : SVType} -> {p : PABasic t} -> (i : SVIntegral t) => State4 i -> State4 $ PT {t} {p}

||| 6.7.1 Net declarations with built-in net types
||| A lexical restriction applies to the use of the reg keyword in a net or port declaration. A net type keyword
||| shall not be followed directly by the reg keyword. Thus, the following declaration is in error:
|||   tri reg r;
public export
data NotReg : SVIntegral svt -> Type where
  NRSL : NotReg $ ST {t=Logic'}
  NRSB : NotReg $ ST {t=Bit'}
  NRPT : {t : SVType} -> {p : PABasic t} -> (i : SVIntegral t) => NotReg i => NotReg $ PT {t} {p}

||| 6.7.1 Net declarations with built-in net types
||| Certain restrictions apply to the data type of a net. A valid data type for a net shall be one of the following:
|||   a) A 4-state integral type, including, for example, a packed array or packed structure (see 6.11.1).
|||   b) A fixed-size unpacked array or unpacked structure or union, where each element has a valid data
|||      type for a net.
public export
data AllowedNetData : SVType -> Type where
  -- Imp : ImplOrPacked Implicit
  NA : (i : SVIntegral obj) => (s : State4 i) => NotReg i => AllowedNetData obj
  NB : AllowedNetData t => AllowedNetData $ UnpackedArr t s e
  -- TODO: unpacked structure, union

public export
data VarOrPacked : SVType -> Type where
  VR : VarOrPacked $ RVar t
  VS : VarOrPacked $ SVar t
  VV : VarOrPacked $ VVar t
  VP : VarOrPacked $ PackedArr p s e

public export
defaultNetType : SVObject
defaultNetType = Net Wire' (SVar Logic') {ald=NA {i=ST {t=Logic'}} {s=SS {t=Logic'}}}

namespace SVObjList

  public export
  data SVObjList = Nil | (::) SVObject SVObjList

  public export
  length : SVObjList -> Nat
  length []      = Z
  length (_::ms) = S $ length ms

  public export %inline
  (.length) : SVObjList -> Nat
  (.length) = length

  public export
  (++) : SVObjList -> SVObjList -> SVObjList
  Nil       ++ ys = ys
  (x :: xs) ++ ys = x :: (xs ++ ys)

  public export
  toList : SVObjList -> List SVObject
  toList []        = []
  toList (x :: xs) = x :: toList xs

  export
  svolistAppendLen : (xs : SVObjList) -> (ys : SVObjList) -> length xs + length ys = length (xs ++ ys)
  svolistAppendLen []        ys = Refl
  svolistAppendLen (_ :: xs) ys = rewrite svolistAppendLen xs ys in Refl

  export
  comPS : {0 a, b: SVObjList} -> (0 m : Nat -> Type) -> m (length a + length b) -> m (length (a ++ b))
  comPS _ v = rewrite sym $ svolistAppendLen a b in v

  export
  comLen : {0 a, b: SVObjList} -> Vect (length a + length b) c -> Vect (length (a ++ b)) c
  comLen = comPS $ \n => Vect n c

  export
  comFin : {0 a, b: SVObjList} -> Fin (length a + length b) -> Fin (length (a ++ b))
  comFin = comPS Fin

  -- Maybe, specialised type `IndexIn : PortsList -> Type` isomorphic to `Fin (length xs)`

  public export
  typeOf : (xs : SVObjList) -> Fin (length xs) -> SVObject
  typeOf (p::_ ) FZ     = p
  typeOf (_::ps) (FS i) = typeOf ps i

namespace ModuleSig

  public export
  record ModuleSig where
    constructor MkModuleSig
    inputs  : SVObjList
    outputs : SVObjList

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
  (++) : ModuleSigsList -> ModuleSigsList -> ModuleSigsList
  Nil       ++ ys = ys
  (x :: xs) ++ ys = x :: (xs ++ ys)

public export
allInputs : {ms : ModuleSigsList} -> FinsList ms.length -> SVObjList
allInputs []      = []
allInputs (i::is) = (index ms i).inputs ++ allInputs is

public export
allOutputs : {ms : ModuleSigsList} -> FinsList ms.length -> SVObjList
allOutputs []      = []
allOutputs (i::is) = (index ms i).outputs ++ allOutputs is
