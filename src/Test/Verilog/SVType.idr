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
data NetType = Supply0' | Supply1' | Triand' | Trior' | Trireg' | Tri0' | Tri1' | Tri' | Uwire' | Wire' | Wand' | Wor';

namespace States
  ||| 6.3.1 Logic values
  |||
  ||| The SystemVerilog value set consists of the following four basic values:
  ||| 0—represents a logic zero or a false condition
  ||| 1—represents a logic one or a true condition
  ||| x—represents an unknown logic value
  ||| z—represents a high-impedance state
  |||
  ||| IEEE 1800-2023
  namespace Logic4
    ||| 0 or 1 or x or z
    public export
    data S4Value = Z | S | X | H

  namespace Logic2
    ||| 0 or 1
    public export
    data S2Value = Z | S

  ||| 6.3.1 Logic values
  |||
  ||| Several SystemVerilog data types are 4-state types, which can store all four logic values. All bits of 4-state
  ||| vectors can be independently set to one of the four basic values. Some SystemVerilog data types are 2-state,
  ||| and only store 0 or 1 values in each bit of a vector. Other exceptions are the event type (see 6.17), which has
  ||| no storage, and the real types (see 6.12).
  public export
  data State = S2 | S4

  public export
  Eq State where
    (==) S2 S2 = True
    (==) S4 S4 = True
    (==) _  _  = False

namespace IntegerAtomType

  ||| 6.9 Vector declarations
  ||| A data object declared as reg, logic, or bit (or as a matching user-defined type or implicitly as logic)
  ||| without a range specification shall be considered 1-bit wide and is known as a scalar. A multibit data object
  ||| of one of these types shall be declared by specifying a range and is known as a vector. Vectors are packed
  ||| arrays of scalars
  public export
  data IntegerAtomType = Byte' | Shortint' | Int' | Longint' | Integer' | Time';

  public export
  Eq IntegerAtomType where
    (==) Byte'     Byte'     = True
    (==) Shortint' Shortint' = True
    (==) Int'      Int'      = True
    (==) Longint'  Longint'  = True
    (==) Integer'  Integer'  = True
    (==) Time'     Time'     = True
    (==) _         _         = False

  public export
  bitsCnt : IntegerAtomType -> Nat
  bitsCnt Byte'     = 8
  bitsCnt Shortint' = 16
  bitsCnt Int'      = 32
  bitsCnt Longint'  = 64
  bitsCnt Integer'  = 32
  bitsCnt Time'     = 64

  public export
  isSigned : IntegerAtomType -> Bool
  isSigned Byte'     = True
  isSigned Shortint' = True
  isSigned Int'      = True
  isSigned Longint'  = True
  isSigned Integer'  = True
  isSigned Time'     = False

  public export
  states : IntegerAtomType -> State
  states Byte'     = S2
  states Shortint' = S2
  states Int'      = S2
  states Longint'  = S2
  states Integer'  = S4
  states Time'     = S4

namespace IntegerVectorType

  public export
  data IntegerVectorType = Bit' | Logic' | Reg';

  public export
  Eq IntegerVectorType where
    (==) Bit'   Bit'   = True
    (==) Logic' Logic' = True
    (==) Reg'   Reg'   = True
    (==) _      _      = False

  public export
  states : IntegerVectorType -> State
  states Bit'   = S2
  states Logic' = S4
  states Reg'   = S4

namespace NonIntegerType

  public export
  data NonIntegerType = Shortreal' | Real' | Realtime';

  public export
  Eq NonIntegerType where
    (==) Shortreal' Shortreal' = True
    (==) Real'      Real'      = True
    (==) Realtime'  Realtime'  = True
    (==) _          _          = False

  public export
  bitsCnt : NonIntegerType -> Nat
  bitsCnt Shortreal' = 32
  bitsCnt Real'      = 64
  bitsCnt Realtime'  = 64

namespace SVType

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
  |||
  public export
  data SVType : Type where
    -- Implicit : SVType -- Declare an implicit net type
    RVar : NonIntegerType -> SVType
    SVar : IntegerVectorType -> SVType
    VVar : IntegerAtomType -> SVType
    PackedArr : (t : SVType) -> (p : PABasic t) => Nat -> Nat -> SVType
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
    -- TODO: Add constructors to `TypeLiteral` when add new types to SVType
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

  ||| 7.4.1 Packed arrays
  ||| Packed arrays can be made of only the single bit data types (bit, logic, reg), enumerated types, and
  ||| recursively other packed arrays and packed structures.
  public export
  data PABasic : SVType -> Type where
    PS : PABasic $ SVar s
    PA : {t : SVType} -> (p : PABasic t) => PABasic $ PackedArr t s e

  ||| 6.11.1 Integral types
  |||
  ||| The term integral is used throughout this standard to refer to the data types that can represent a single basic
  ||| integer data type, packed array, packed structure, packed union, or enum.
  public export
  data SVIntegral : SVType -> Type where
    ST : SVIntegral $ SVar t
    VT : SVIntegral $ VVar t
    PT : {t : SVType} -> (p : PABasic t) => SVIntegral $ PackedArr t s e
    -- Packed struct, union, enum

  public export
  data State4S : IntegerVectorType -> Type where
    S4L : State4S Logic'
    S4R : State4S Reg'

  public export
  data State4V : IntegerAtomType -> Type where
    V4I : State4V Integer'
    V4T : State4V Time'

  public export
  data State4 : SVIntegral svt -> Type where
    SS : State4S t => State4 $ ST {t}
    SV : State4V t => State4 $ VT {t}
    SP : {t : SVType} -> (p : PABasic t) => (i : SVIntegral t) => State4 i -> State4 $ PT {t}

  ||| 6.7.1 Net declarations with built-in net types
  ||| A lexical restriction applies to the use of the reg keyword in a net or port declaration. A net type keyword
  ||| shall not be followed directly by the reg keyword. Thus, the following declaration is in error:
  |||   tri reg r;
  public export
  data NotReg : SVIntegral svt -> Type where
    NRSL : NotReg $ ST {t=Logic'}
    NRSB : NotReg $ ST {t=Bit'}
    NRPT : {t : SVType} -> (p : PABasic t) => (i : SVIntegral t) => NotReg i => NotReg $ PT {t}

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
  bitsCnt : SVType -> Nat
  bitsCnt (RVar x)            = bitsCnt x
  bitsCnt (SVar x)            = 1
  bitsCnt (VVar x)            = bitsCnt x
  bitsCnt (PackedArr   t s e) = S (max s e `minus` min s e) * bitsCnt t
  bitsCnt (UnpackedArr t s e) = bitsCnt t

  public export
  isSigned : SVType -> Bool
  isSigned (RVar x)            = True
  isSigned (SVar x)            = False
  isSigned (VVar x)            = isSigned x
  isSigned (PackedArr   t _ _) = isSigned t
  isSigned (UnpackedArr t _ _) = isSigned t

  public export
  states : SVType -> State
  states (RVar x)            = S4
  states (SVar x)            = states x
  states (VVar x)            = states x
  states (PackedArr   t s e) = states t
  states (UnpackedArr t s e) = states t

namespace SVObject

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
    Net : NetType -> (t : SVType) -> (p : AllowedNetData t) => SVObject
    Var : SVType -> SVObject

  public export
  bitsCnt : SVObject -> Nat
  bitsCnt (Net _ t) = bitsCnt t
  bitsCnt (Var   t) = bitsCnt t

  ||| 6.8 Variable Declarations
  |||
  ||| The byte, shortint, int, integer, and longint types are signed types by default. Other net and
  ||| variable types can be explicitly declared as signed.
  public export
  isSigned : SVObject -> Bool
  isSigned (Net _ _) = False
  isSigned (Var x)   = isSigned x

  ||| 6.7.1 Net declarations with built-in net types
  ||| A valid data type for a net shall be one of the following:
  ||| a) 4-state integral type ...
  public export
  states : SVObject -> State
  states (Net _ t) = S4
  states (Var   t) = states t

  public export
  valueOf : SVObject -> SVType
  valueOf (Net _ t) = t
  valueOf (Var   t) = t

||| 6.6.2 Unresolved nets
||| The uwire net is an unresolved or unidriver wire and is used to model nets that allow only a single driver.
public export
data ResolvedNet : SVObject -> Type where
  NS0 : {t : SVType} -> (p : AllowedNetData t) => ResolvedNet $ Net Supply0' t
  NS1 : {t : SVType} -> (p : AllowedNetData t) => ResolvedNet $ Net Supply1' t
  NTD : {t : SVType} -> (p : AllowedNetData t) => ResolvedNet $ Net Triand' t
  NTR : {t : SVType} -> (p : AllowedNetData t) => ResolvedNet $ Net Trior' t
  NTG : {t : SVType} -> (p : AllowedNetData t) => ResolvedNet $ Net Trireg' t
  NT0 : {t : SVType} -> (p : AllowedNetData t) => ResolvedNet $ Net Tri0' t
  NT1 : {t : SVType} -> (p : AllowedNetData t) => ResolvedNet $ Net Tri1' t
  NWI : {t : SVType} -> (p : AllowedNetData t) => ResolvedNet $ Net Wire' t
  NTI : {t : SVType} -> (p : AllowedNetData t) => ResolvedNet $ Net Tri' t
  NWA : {t : SVType} -> (p : AllowedNetData t) => ResolvedNet $ Net Wand' t
  NWO : {t : SVType} -> (p : AllowedNetData t) => ResolvedNet $ Net Wor' t

public export
data VarOrPacked : SVType -> Type where
  VR : VarOrPacked $ RVar t
  VS : VarOrPacked $ SVar t
  VV : VarOrPacked $ VVar t
  VP : {t : SVType} -> (p : PABasic t) => VarOrPacked $ PackedArr t s e

public export
data IsUnpackedArr : SVType -> Type where
  IUA : IsUnpackedArr $ UnpackedArr t s e

public export
defaultNetType : SVObject
defaultNetType = Net Wire' (SVar Logic') -- {p=NA {i=ST {t=Logic'}} {s=SS {t=Logic'}}}

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

  public export
  fromList : List SVObject -> SVObjList
  fromList [] = []
  fromList (x :: xs) = x :: fromList xs

  public export
  reverse : SVObjList -> SVObjList
  reverse svl = fromList $ reverse $ toList svl

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

public export
isUnpacked' : SVType -> Bool
isUnpacked' (UnpackedArr _ _ _) = True
isUnpacked' _                   = False

public export
isUnpacked : SVObject -> Bool
isUnpacked (Net _ t) = isUnpacked' t
isUnpacked (Var   t) = isUnpacked' t
