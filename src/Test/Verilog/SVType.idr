module Test.Verilog.SVType

import Data.Fuel
import Data.Vect

import Data.List

import Decidable.Equality

import Decidable.Decidable

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

DecEq NetType where
  decEq Supply0' Supply0' = Yes Refl
  decEq Supply0' Supply1' = No  $ \Refl impossible
  decEq Supply0' Triand'  = No  $ \Refl impossible
  decEq Supply0' Trior'   = No  $ \Refl impossible
  decEq Supply0' Trireg'  = No  $ \Refl impossible
  decEq Supply0' Tri0'    = No  $ \Refl impossible
  decEq Supply0' Tri1'    = No  $ \Refl impossible
  decEq Supply0' Tri'     = No  $ \Refl impossible
  decEq Supply0' Uwire'   = No  $ \Refl impossible
  decEq Supply0' Wire'    = No  $ \Refl impossible
  decEq Supply0' Wand'    = No  $ \Refl impossible
  decEq Supply0' Wor'     = No  $ \Refl impossible
  decEq Supply1' Supply0' = No  $ \Refl impossible
  decEq Supply1' Supply1' = Yes Refl
  decEq Supply1' Triand'  = No  $ \Refl impossible
  decEq Supply1' Trior'   = No  $ \Refl impossible
  decEq Supply1' Trireg'  = No  $ \Refl impossible
  decEq Supply1' Tri0'    = No  $ \Refl impossible
  decEq Supply1' Tri1'    = No  $ \Refl impossible
  decEq Supply1' Tri'    =  No  $ \Refl impossible
  decEq Supply1' Uwire'   = No  $ \Refl impossible
  decEq Supply1' Wire'    = No  $ \Refl impossible
  decEq Supply1' Wand'    = No  $ \Refl impossible
  decEq Supply1' Wor'     = No  $ \Refl impossible
  decEq Triand'  Supply0' = No  $ \Refl impossible
  decEq Triand'  Supply1' = No  $ \Refl impossible
  decEq Triand'  Triand'  = Yes Refl
  decEq Triand'  Trior'   = No  $ \Refl impossible
  decEq Triand'  Trireg'  = No  $ \Refl impossible
  decEq Triand'  Tri0'    = No  $ \Refl impossible
  decEq Triand'  Tri1'    = No  $ \Refl impossible
  decEq Triand'  Tri'     = No  $ \Refl impossible
  decEq Triand'  Uwire'   = No  $ \Refl impossible
  decEq Triand'  Wire'    = No  $ \Refl impossible
  decEq Triand'  Wand'    = No  $ \Refl impossible
  decEq Triand'  Wor'     = No  $ \Refl impossible
  decEq Trior'   Supply0' = No  $ \Refl impossible
  decEq Trior'   Supply1' = No  $ \Refl impossible
  decEq Trior'   Triand'  = No  $ \Refl impossible
  decEq Trior'   Trior'   = Yes Refl
  decEq Trior'   Trireg'  = No  $ \Refl impossible
  decEq Trior'   Tri0'    = No  $ \Refl impossible
  decEq Trior'   Tri1'    = No  $ \Refl impossible
  decEq Trior'   Tri'     = No  $ \Refl impossible
  decEq Trior'   Uwire'   = No  $ \Refl impossible
  decEq Trior'   Wire'    = No  $ \Refl impossible
  decEq Trior'   Wand'    = No  $ \Refl impossible
  decEq Trior'   Wor'     = No  $ \Refl impossible
  decEq Trireg'  Supply0' = No  $ \Refl impossible
  decEq Trireg'  Supply1' = No  $ \Refl impossible
  decEq Trireg'  Triand'  = No  $ \Refl impossible
  decEq Trireg'  Trior'   = No  $ \Refl impossible
  decEq Trireg'  Trireg'  = Yes Refl
  decEq Trireg'  Tri0'    = No  $ \Refl impossible
  decEq Trireg'  Tri1'    = No  $ \Refl impossible
  decEq Trireg'  Tri'     = No  $ \Refl impossible
  decEq Trireg'  Uwire'   = No  $ \Refl impossible
  decEq Trireg'  Wire'    = No  $ \Refl impossible
  decEq Trireg'  Wand'    = No  $ \Refl impossible
  decEq Trireg'  Wor'     = No  $ \Refl impossible
  decEq Tri0'    Supply0' = No  $ \Refl impossible
  decEq Tri0'    Supply1' = No  $ \Refl impossible
  decEq Tri0'    Triand'  = No  $ \Refl impossible
  decEq Tri0'    Trior'   = No  $ \Refl impossible
  decEq Tri0'    Trireg'  = No  $ \Refl impossible
  decEq Tri0'    Tri0'    = Yes Refl
  decEq Tri0'    Tri1'    = No  $ \Refl impossible
  decEq Tri0'    Tri'     = No  $ \Refl impossible
  decEq Tri0'    Uwire'   = No  $ \Refl impossible
  decEq Tri0'    Wire'    = No  $ \Refl impossible
  decEq Tri0'    Wand'    = No  $ \Refl impossible
  decEq Tri0'    Wor'     = No  $ \Refl impossible
  decEq Tri1'    Supply0' = No  $ \Refl impossible
  decEq Tri1'    Supply1' = No  $ \Refl impossible
  decEq Tri1'    Triand'  = No  $ \Refl impossible
  decEq Tri1'    Trior'   = No  $ \Refl impossible
  decEq Tri1'    Trireg'  = No  $ \Refl impossible
  decEq Tri1'    Tri0'    = No  $ \Refl impossible
  decEq Tri1'    Tri1'    = Yes Refl
  decEq Tri1'    Tri'     = No  $ \Refl impossible
  decEq Tri1'    Uwire'   = No  $ \Refl impossible
  decEq Tri1'    Wire'    = No  $ \Refl impossible
  decEq Tri1'    Wand'    = No  $ \Refl impossible
  decEq Tri1'    Wor'     = No  $ \Refl impossible
  decEq Tri'     Supply0' = No  $ \Refl impossible
  decEq Tri'     Supply1' = No  $ \Refl impossible
  decEq Tri'     Triand'  = No  $ \Refl impossible
  decEq Tri'     Trior'   = No  $ \Refl impossible
  decEq Tri'     Trireg'  = No  $ \Refl impossible
  decEq Tri'     Tri0'    = No  $ \Refl impossible
  decEq Tri'     Tri1'    = No  $ \Refl impossible
  decEq Tri'     Tri'     = Yes Refl
  decEq Tri'     Uwire'   = No  $ \Refl impossible
  decEq Tri'     Wire'    = No  $ \Refl impossible
  decEq Tri'     Wand'    = No  $ \Refl impossible
  decEq Tri'     Wor'     = No  $ \Refl impossible
  decEq Uwire'   Supply0' = No  $ \Refl impossible
  decEq Uwire'   Supply1' = No  $ \Refl impossible
  decEq Uwire'   Triand'  = No  $ \Refl impossible
  decEq Uwire'   Trior'   = No  $ \Refl impossible
  decEq Uwire'   Trireg'  = No  $ \Refl impossible
  decEq Uwire'   Tri0'    = No  $ \Refl impossible
  decEq Uwire'   Tri1'    = No  $ \Refl impossible
  decEq Uwire'   Tri'     = No  $ \Refl impossible
  decEq Uwire'   Uwire'   = Yes Refl
  decEq Uwire'   Wire'    = No  $ \Refl impossible
  decEq Uwire'   Wand'    = No  $ \Refl impossible
  decEq Uwire'   Wor'     = No  $ \Refl impossible
  decEq Wire'    Supply0' = No  $ \Refl impossible
  decEq Wire'    Supply1' = No  $ \Refl impossible
  decEq Wire'    Triand'  = No  $ \Refl impossible
  decEq Wire'    Trior'   = No  $ \Refl impossible
  decEq Wire'    Trireg'  = No  $ \Refl impossible
  decEq Wire'    Tri0'    = No  $ \Refl impossible
  decEq Wire'    Tri1'    = No  $ \Refl impossible
  decEq Wire'    Tri'     = No  $ \Refl impossible
  decEq Wire'    Uwire'   = No  $ \Refl impossible
  decEq Wire'    Wire'    = Yes Refl
  decEq Wire'    Wand'    = No  $ \Refl impossible
  decEq Wire'    Wor'     = No  $ \Refl impossible
  decEq Wand'    Supply0' = No  $ \Refl impossible
  decEq Wand'    Supply1' = No  $ \Refl impossible
  decEq Wand'    Triand'  = No  $ \Refl impossible
  decEq Wand'    Trior'   = No  $ \Refl impossible
  decEq Wand'    Trireg'  = No  $ \Refl impossible
  decEq Wand'    Tri0'    = No  $ \Refl impossible
  decEq Wand'    Tri1'    = No  $ \Refl impossible
  decEq Wand'    Tri'     = No  $ \Refl impossible
  decEq Wand'    Uwire'   = No  $ \Refl impossible
  decEq Wand'    Wire'    = No  $ \Refl impossible
  decEq Wand'    Wand'    = Yes Refl
  decEq Wand'    Wor'     = No  $ \Refl impossible
  decEq Wor'     Supply0' = No  $ \Refl impossible
  decEq Wor'     Supply1' = No  $ \Refl impossible
  decEq Wor'     Triand'  = No  $ \Refl impossible
  decEq Wor'     Trior'   = No  $ \Refl impossible
  decEq Wor'     Trireg'  = No  $ \Refl impossible
  decEq Wor'     Tri0'    = No  $ \Refl impossible
  decEq Wor'     Tri1'    = No  $ \Refl impossible
  decEq Wor'     Tri'     = No  $ \Refl impossible
  decEq Wor'     Uwire'   = No  $ \Refl impossible
  decEq Wor'     Wire'    = No  $ \Refl impossible
  decEq Wor'     Wand'    = No  $ \Refl impossible
  decEq Wor'     Wor'     = Yes Refl

Eq NetType where
  (==) a b = isYes $ decEq a b

namespace States
  ||| 6.3.1 Logic values
  |||
  ||| Several SystemVerilog data types are 4-state types, which can store all four logic values. All bits of 4-state
  ||| vectors can be independently set to one of the four basic values. Some SystemVerilog data types are 2-state,
  ||| and only store 0 or 1 values in each bit of a vector. Other exceptions are the event type (see 6.17), which has
  ||| no storage, and the real types (see 6.12).
  public export
  data State = S2 | S4

  DecEq State where
    decEq S2 S2 = Yes Refl
    decEq S2 S4 = No  $ \Refl impossible
    decEq S4 S2 = No  $ \Refl impossible
    decEq S4 S4 = Yes Refl

  public export
  Eq State where
    (==) a b = isYes $ decEq a b

namespace IntegerAtomType

  ||| 6.9 Vector declarations
  ||| A data object declared as reg, logic, or bit (or as a matching user-defined type or implicitly as logic)
  ||| without a range specification shall be considered 1-bit wide and is known as a scalar. A multibit data object
  ||| of one of these types shall be declared by specifying a range and is known as a vector. Vectors are packed
  ||| arrays of scalars
  public export
  data IntegerAtomType = Byte' | Shortint' | Int' | Longint' | Integer' | Time';

  public export
  DecEq IntegerAtomType where
    decEq Byte'     Byte'     = Yes Refl
    decEq Byte'     Shortint' = No $ \Refl impossible
    decEq Byte'     Int'      = No $ \Refl impossible
    decEq Byte'     Longint'  = No $ \Refl impossible
    decEq Byte'     Integer'  = No $ \Refl impossible
    decEq Byte'     Time'     = No $ \Refl impossible
    decEq Shortint' Byte'     = No $ \Refl impossible
    decEq Shortint' Shortint' = Yes Refl
    decEq Shortint' Int'      = No $ \Refl impossible
    decEq Shortint' Longint'  = No $ \Refl impossible
    decEq Shortint' Integer'  = No $ \Refl impossible
    decEq Shortint' Time'     = No $ \Refl impossible
    decEq Int'      Byte'     = No $ \Refl impossible
    decEq Int'      Shortint' = No $ \Refl impossible
    decEq Int'      Int'      = Yes Refl
    decEq Int'      Longint'  = No $ \Refl impossible
    decEq Int'      Integer'  = No $ \Refl impossible
    decEq Int'      Time'     = No $ \Refl impossible
    decEq Longint'  Byte'     = No $ \Refl impossible
    decEq Longint'  Shortint' = No $ \Refl impossible
    decEq Longint'  Int'      = No $ \Refl impossible
    decEq Longint'  Longint'  = Yes Refl
    decEq Longint'  Integer'  = No $ \Refl impossible
    decEq Longint'  Time'     = No $ \Refl impossible
    decEq Integer'  Byte'     = No $ \Refl impossible
    decEq Integer'  Shortint' = No $ \Refl impossible
    decEq Integer'  Int'      = No $ \Refl impossible
    decEq Integer'  Longint'  = No $ \Refl impossible
    decEq Integer'  Integer'  = Yes Refl
    decEq Integer'  Time'     = No $ \Refl impossible
    decEq Time'     Byte'     = No $ \Refl impossible
    decEq Time'     Shortint' = No $ \Refl impossible
    decEq Time'     Int'      = No $ \Refl impossible
    decEq Time'     Longint'  = No $ \Refl impossible
    decEq Time'     Integer'  = No $ \Refl impossible
    decEq Time'     Time'     = Yes Refl

  public export
  Eq IntegerAtomType where
   (==) a b = isYes $ decEq a b

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
  DecEq IntegerVectorType where
    decEq Bit'   Bit'   = Yes Refl
    decEq Bit'   Logic' = No $ \Refl impossible
    decEq Bit'   Reg'   = No $ \Refl impossible
    decEq Logic' Bit'   = No $ \Refl impossible
    decEq Logic' Logic' = Yes Refl
    decEq Logic' Reg'   = No $ \Refl impossible
    decEq Reg'   Bit'   = No $ \Refl impossible
    decEq Reg'   Logic' = No $ \Refl impossible
    decEq Reg'   Reg'   = Yes Refl

  public export
  Eq IntegerVectorType where
   (==) a b = isYes $ decEq a b

  public export
  states : IntegerVectorType -> State
  states Bit'   = S2
  states Logic' = S4
  states Reg'   = S4

namespace NonIntegerType

  public export
  data NonIntegerType = Shortreal' | Real' | Realtime';

  public export
  DecEq NonIntegerType where
    decEq Shortreal' Shortreal' = Yes Refl
    decEq Shortreal' Real'      = No $ \Refl impossible
    decEq Shortreal' Realtime'  = No $ \Refl impossible
    decEq Real' Shortreal'      = No $ \Refl impossible
    decEq Real' Real'           = Yes Refl
    decEq Real' Realtime'       = No $ \Refl impossible
    decEq Realtime' Shortreal'  = No $ \Refl impossible
    decEq Realtime' Real'       = No $ \Refl impossible
    decEq Realtime' Realtime'   = Yes Refl

  public export
  Eq NonIntegerType where
    (==) a b = isYes $ decEq a b

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

  public export
  DecEq (PABasic t)

  public export
  DecEq SVType where
    decEq (RVar x) (RVar y) with (decEq x y)
      decEq (RVar x) (RVar x) | (Yes Refl) = Yes Refl
      decEq (RVar x) (RVar y) | (No contra) = No $ \Refl => contra Refl
    decEq (RVar x) (SVar y) = No $ \Refl impossible
    decEq (RVar x) (VVar y) = No $ \Refl impossible
    decEq (RVar x) (PackedArr t k j) = No $ \Refl impossible
    decEq (RVar x) (UnpackedArr y k j) = No $ \Refl impossible
    decEq (SVar x) (RVar y) = No $ \Refl impossible
    decEq (SVar x) (SVar y) with (decEq x y)
      decEq (SVar x) (SVar x) | (Yes Refl) = Yes Refl
      decEq (SVar x) (SVar y) | (No contra) = No $ \Refl => contra Refl
    decEq (SVar x) (VVar y) = No $ \Refl impossible
    decEq (SVar x) (PackedArr t k j) = No $ \Refl impossible
    decEq (SVar x) (UnpackedArr y k j) = No $ \Refl impossible
    decEq (VVar x) (RVar y) = No $ \Refl impossible
    decEq (VVar x) (SVar y) = No $ \Refl impossible
    decEq (VVar x) (VVar y) with (decEq x y)
      decEq (VVar x) (VVar x) | (Yes Refl) = Yes Refl
      decEq (VVar x) (VVar y) | (No contra) = No $ \Refl => contra Refl
    decEq (VVar x) (PackedArr t k j) = No $ \Refl impossible
    decEq (VVar x) (UnpackedArr y k j) = No $ \Refl impossible
    decEq (PackedArr t k j) (RVar x) = No $ \Refl impossible
    decEq (PackedArr t k j) (SVar x) = No $ \Refl impossible
    decEq (PackedArr t k j) (VVar x) = No $ \Refl impossible
    decEq (PackedArr t {p} k j) (PackedArr t' {p = p'} k' j') with (decEq k k')
      decEq (PackedArr t {p} k j) (PackedArr t' {p = p'} k j') | (Yes Refl) with (decEq j j')
        decEq (PackedArr t {p} k j) (PackedArr t' {p = p'} k j) | (Yes Refl) | (Yes Refl) with (decEq t t')
          decEq (PackedArr t {p} k j) (PackedArr t {p = p'} k j) | (Yes Refl) | (Yes Refl) | (Yes Refl) with (decEq p p')
            decEq (PackedArr t {p} k j) (PackedArr t {p} k j) | (Yes Refl) | (Yes Refl) | (Yes Refl) | (Yes Refl) = Yes Refl
            decEq (PackedArr t {p} k j) (PackedArr t {p = p'} k j) | (Yes Refl) | (Yes Refl) | (Yes Refl) | (No contra) = No $ \Refl => contra Refl
          decEq (PackedArr t {p} k j) (PackedArr t' {p = p'} k j) | (Yes Refl) | (Yes Refl) | (No contra) = No $ \Refl => contra Refl
        decEq (PackedArr t k j) (PackedArr t' k j') | (Yes Refl) | (No contra) = No $ \Refl => contra Refl
      decEq (PackedArr t k j) (PackedArr t' k' j') | (No contra) = No $ \Refl => contra Refl
    decEq (PackedArr t k j) (UnpackedArr x i k1) = No $ \Refl impossible
    decEq (UnpackedArr x k j) (RVar y) = No $ \Refl impossible
    decEq (UnpackedArr x k j) (SVar y) = No $ \Refl impossible
    decEq (UnpackedArr x k j) (VVar y) = No $ \Refl impossible
    decEq (UnpackedArr x k j) (PackedArr t i k1) = No $ \Refl impossible
    decEq (UnpackedArr x k j) (UnpackedArr x' k' j') with (decEq k k')
      decEq (UnpackedArr x k j) (UnpackedArr x' k j') | (Yes Refl) with (decEq j j')
        decEq (UnpackedArr x k j) (UnpackedArr x' k j) | (Yes Refl) | (Yes Refl) with (decEq x x')
          decEq (UnpackedArr x k j) (UnpackedArr x k j) | (Yes Refl) | (Yes Refl) | (Yes Refl) = Yes Refl
          decEq (UnpackedArr x k j) (UnpackedArr x' k j') | (Yes Refl) | (Yes Refl) | (No contra) = No $ \Refl => contra Refl
        decEq (UnpackedArr x k j) (UnpackedArr x' k' j') | (Yes Refl) | (No contra) = No $ \Refl => contra Refl
      decEq (UnpackedArr x k j) (UnpackedArr x' k' j') | (No contra) = No $ \Refl => contra Refl

  public export
  Eq SVType where
    (==) a b = isYes $ decEq a b

  ||| 7.4.1 Packed arrays
  ||| Packed arrays can be made of only the single bit data types (bit, logic, reg), enumerated types, and
  ||| recursively other packed arrays and packed structures.
  public export
  data PABasic : SVType -> Type where
    PS : PABasic $ SVar s
    PA : {t : SVType} -> (p : PABasic t) => PABasic $ PackedArr t s e

  public export
  DecEq (PABasic t) where
   decEq PS PS = Yes Refl
   decEq PA PA = Yes Refl

  public export
  Eq (PABasic t) where
    (==) a b = isYes $ decEq a b

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
  DecEq (SVIntegral t) where
    decEq ST ST = Yes Refl
    decEq VT VT = Yes Refl
    decEq PT PT = Yes Refl

  SVIntegralIsSingleton : (t : SVType) -> (a : SVIntegral t) -> (b : SVIntegral t) -> a === b
  SVIntegralIsSingleton (SVar t) ST ST = Refl
  SVIntegralIsSingleton (VVar t) VT VT = Refl
  SVIntegralIsSingleton (PackedArr t s e) PT PT = Refl

  public export
  data State4S : IntegerVectorType -> Type where
    S4L : State4S Logic'
    S4R : State4S Reg'

  public export
  DecEq (State4S t) where
    decEq S4L S4L = Yes Refl
    decEq S4R S4R = Yes Refl

  public export
  data State4V : IntegerAtomType -> Type where
    V4I : State4V Integer'
    V4T : State4V Time'

  public export
  DecEq (State4V t) where
    decEq V4I V4I = Yes Refl
    decEq V4T V4T = Yes Refl

  public export
  data State4 : SVIntegral svt -> Type where
    SS : State4S t => State4 $ ST {t}
    SV : State4V t => State4 $ VT {t}
    SP : {t : SVType} -> (p : PABasic t) => (i : SVIntegral t) => State4 i -> State4 $ PT {t}

  public export
  DecEq (State4 t) where
    decEq (SS @{a}) (SS @{b}) with (decEq a b)
      decEq (SS @{a}) (SS @{a}) | (Yes Refl) = Yes Refl
      decEq (SS @{a}) (SS @{b}) | (No contra) = No $ \Refl => contra Refl
    decEq (SV @{a}) (SV @{b}) with (decEq a b)
      decEq (SV @{a}) (SV @{a}) | (Yes Refl) = Yes Refl
      decEq (SV @{a}) (SV @{b}) | (No contra) = No $ \Refl => contra Refl
    decEq (SP @{p} @{i} x) (SP @{p} @{i'} y) with (decEq i i')
      decEq (SP @{p} @{i} x) (SP @{p} @{i} y) | (Yes Refl) with (decEq x y)
        decEq (SP @{i} x) (SP @{i} x) | (Yes Refl) | (Yes Refl) = Yes Refl
        decEq (SP @{i} x) (SP @{i} y) | (Yes Refl) | (No contra) = No $ \Refl => contra Refl
      decEq (SP @{i} x) (SP @{i'} y) | (No contra) = No $ \Refl => contra Refl

  public export
  data State2S : IntegerVectorType -> Type where
    S2B : State2S Bit'

  public export
  data State2V : IntegerAtomType -> Type where
    V2Int : State2V Int'
    V2Long : State2V Longint'
    V2Short : State2V Shortint'
    V2Byte : State2V Byte'

  public export
  data State2 : SVIntegral svt -> Type where
    SB : State2S t => State2 $ ST {t}
    SV2 : State2V t => State2 $ VT {t}
    SP2 : {t : SVType} -> {p : PABasic t} -> (i : SVIntegral t) => State2 i -> State2 $ PT {t} {p}

  EitherState : (svt : SVType) -> (t : SVIntegral svt) -> State2 t -> State4 t -> Void
  EitherState (SVar Bit') ST SB SS impossible
  EitherState (SVar Logic') ST SB SS impossible
  EitherState (SVar Reg') ST SB SS impossible
  EitherState (VVar Byte') VT SV2 SV impossible
  EitherState (VVar Shortint') VT SV2 SV impossible
  EitherState (VVar Int') VT SV2 SV impossible
  EitherState (VVar Longint') VT SV2 SV impossible
  EitherState (VVar Integer') VT SV2 SV impossible
  EitherState (VVar Time') VT SV2 SV impossible
  EitherState (PackedArr t s e) PT (SP2 {i} x) (SP {i = i'} y) with (SVIntegralIsSingleton t i i')
    EitherState (PackedArr t s e) PT (SP2 {i} x) (SP {i} y) | Refl = EitherState t i x y

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

  DecEq (AllowedNetData t) where
    decEq (NA @{i} @{s} @{nReg}) (NA @{i'} @{s'} @{nReg'}) with (decEq i i')
      decEq (NA @{i} @{s} @{nReg}) (NA @{i} @{s'} @{nReg'}) | (Yes Refl) with (decEq s s')
        decEq (NA @{i} @{s} @{nReg}) (NA @{i} @{s} @{nReg'}) | (Yes Refl) | (Yes Refl)  = ?ggg
        decEq (NA @{i} @{s} @{nReg}) (NA @{i} @{s'} @{nReg'}) | (Yes Refl) | (No contra) = No $ \Refl => contra Refl
      decEq (NA @{i} @{s} @{nReg}) (NA @{i'} @{s'} @{nReg'}) | (No contra) = No $ \Refl => contra Refl
    decEq (NA @{i} @{s} @{nReg}) NB = No $ \Refl impossible
    decEq (NB @{rq}) NA = No $ \Refl impossible
    decEq (NB @{rq}) (NB @{rq'}) with (decEq rq rq')
      decEq (NB @{rq}) (NB @{rq}) | (Yes Refl) = Yes Refl
      decEq (NB @{rq}) (NB @{rq'}) | (No contra) = No $ \Refl => contra Refl

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

  public export
  boundsLen : Nat -> Nat -> Nat
  boundsLen a b = (max a b) `minus` (min a b) + 1

  public export
  (.bitLength) : (t : SVType) -> SVIntegral t => Nat
  (.bitLength) (SVar _) = 1
  (.bitLength) (VVar Int') = 32
  (.bitLength) (VVar Integer') = 32
  (.bitLength) (VVar Longint') = 64
  (.bitLength) (VVar Shortint') = 16
  (.bitLength) (VVar Byte') = 8
  (.bitLength) (VVar Time') = 64
  (.bitLength) (PackedArr t@(SVar _) {p = PS} s e) = (boundsLen s e) * t.bitLength
  (.bitLength) (PackedArr t@(PackedArr _ _ _) {p = PA} s e) = (boundsLen s e) * t.bitLength

  public export
  data NatEq : Nat -> Nat -> Type where
    ReflNat : NatEq n n

  NatEqToEq : {n, m : _} -> n `NatEq` m -> n === m
  NatEqToEq ReflNat = Refl

  EqToNatEq : n === m -> n `NatEq` m
  EqToNatEq Refl = ReflNat

  public export
  data IsEquivalentTo : SVType -> SVType -> Type where
    EquivReflex : IsEquivalentTo t t
    EquivIntegrals2 : (a' : SVIntegral a) => (b' : SVIntegral b) =>
                      State2 a' => State2 b' => (be : a.bitLength `NatEq` b.bitLength) =>
                      IsEquivalentTo a b
    EquivIntegrals4 : (a' : SVIntegral a) => (b' : SVIntegral b) =>
                      State4 a' => State4 b' => (be : a.bitLength `NatEq` b.bitLength) =>
                      IsEquivalentTo a b
    EquivUnpacked : (equiv : IsEquivalentTo a b) => (be : (boundsLen s1 e1) `NatEq` (boundsLen s2 e2)) =>
                    IsEquivalentTo (UnpackedArr a s1 e1) (UnpackedArr b s2 e2)

  0 IsEquivalentToSym : {a, b : SVType} -> IsEquivalentTo a b -> IsEquivalentTo b a
  IsEquivalentToSym {a} {b = a} EquivReflex = EquivReflex
  IsEquivalentToSym {a} {b} (EquivIntegrals2 {be}) = EquivIntegrals2 {be = EqToNatEq $ sym $ NatEqToEq be}
  IsEquivalentToSym {a} {b} (EquivIntegrals4 {be}) = EquivIntegrals4 {be = EqToNatEq $ sym $ NatEqToEq be}
  IsEquivalentToSym {a = (UnpackedArr a s1 e1)} {b = (UnpackedArr b s2 e2)} (EquivUnpacked {equiv} {be}) =
    EquivUnpacked {equiv = IsEquivalentToSym equiv} {be = EqToNatEq $ sym $ NatEqToEq be}

  0 IsEquivalentToTrans : (a, b, c : SVType) -> IsEquivalentTo a b -> IsEquivalentTo b c -> IsEquivalentTo a c
  IsEquivalentToTrans a a c EquivReflex p2 = p2
  IsEquivalentToTrans a b b EquivIntegrals2 EquivReflex = EquivIntegrals2
  IsEquivalentToTrans a b c (EquivIntegrals2 {b'} {be = be1}) (EquivIntegrals2 {a' = b''} {be = be2}) with (SVIntegralIsSingleton b b' b'')
    IsEquivalentToTrans a b c (EquivIntegrals2 {b'} {be = be1}) (EquivIntegrals2 {a' = b'} {be = be2}) | Refl =
      EquivIntegrals2 {be = EqToNatEq $ trans (NatEqToEq be1) (NatEqToEq be2)}
  IsEquivalentToTrans a b c (EquivIntegrals2 @{_} @{b'} @{_} @{u}) (EquivIntegrals4 @{b''} @{_} @{u'}) with (SVIntegralIsSingleton b b' b'')
    IsEquivalentToTrans a b c (EquivIntegrals2 @{_} @{b''} @{_} @{u}) (EquivIntegrals4 @{b''} @{_} @{u'}) | Refl = absurd $ EitherState b b'' u u'
  IsEquivalentToTrans a b b EquivIntegrals4 EquivReflex = EquivIntegrals4
  IsEquivalentToTrans a b c (EquivIntegrals4 @{_} @{b'} @{_} @{u}) (EquivIntegrals2 @{b''} @{_} @{u'}) with (SVIntegralIsSingleton b b' b'')
    IsEquivalentToTrans a b c (EquivIntegrals4 @{_} @{b''} @{_} @{u}) (EquivIntegrals2 @{b''} @{_} @{u'}) | Refl = absurd $ EitherState b b'' u' u
  IsEquivalentToTrans a b c (EquivIntegrals4 {be} {b'}) (EquivIntegrals4 {be = be'} {a' = b''}) with (SVIntegralIsSingleton b b' b'')
    IsEquivalentToTrans a b c (EquivIntegrals4 {be} {b'}) (EquivIntegrals4 {be = be'} {a' = b'}) | Refl =
      EquivIntegrals4 {be = EqToNatEq $ trans (NatEqToEq be) (NatEqToEq be')}
  IsEquivalentToTrans (UnpackedArr a s1 e1) (UnpackedArr b s2 e2) (UnpackedArr b s2 e2) EquivUnpacked EquivReflex = EquivUnpacked
  IsEquivalentToTrans (UnpackedArr a s1 e1) (UnpackedArr b s2 e2) c EquivUnpacked EquivIntegrals4 impossible
  IsEquivalentToTrans (UnpackedArr a s1 e1) (UnpackedArr b s2 e2) (UnpackedArr c s3 e3)
                      (EquivUnpacked {be} {equiv}) (EquivUnpacked {be = be'} {equiv = equiv'}) =
                      EquivUnpacked {be = EqToNatEq $ trans (NatEqToEq be) (NatEqToEq be')} {equiv = IsEquivalentToTrans a b c equiv equiv'}

  -- For now the definition is quite trivial, but we will need to expand it for unpacked structures, dynamic arrays, etc.
  public export
  data IsAssignableTo : SVType -> SVType -> Type where
    EquivalentsAreAssignable : IsEquivalentTo a b -> IsAssignableTo a b
    IntegralsAreAssignable : (SVIntegral a) => (SVIntegral b) => IsAssignableTo a b

  0 IsAssignableReflexive : (a : SVType) -> IsAssignableTo a a
  IsAssignableReflexive a = EquivalentsAreAssignable {a} {b = a} EquivReflex

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

  public export
  DecEq (AllowedNetData t)

  public export
  DecEq SVObject where
    decEq (Net x t @{ald} ) (Net x' t' @{ald'}) with (decEq x x')
      decEq (Net x t @{ald} ) (Net x t' @{ald'}) | (Yes Refl) with (decEq t t')
        decEq (Net x t @{ald} ) (Net x t @{ald'}) | (Yes Refl) | (Yes Refl) with (decEq ald ald')
          decEq (Net x t @{ald} ) (Net x t @{ald}) | (Yes Refl) | (Yes Refl) | (Yes Refl) = Yes Refl
          decEq (Net x t @{ald} ) (Net x' t @{ald'}) | (Yes Refl) | (Yes Refl) | (No contra) = No $ \Refl => contra Refl
        decEq (Net x t @{ald} ) (Net x' t' @{ald'}) | (Yes Refl) | (No contra) = No $ \Refl => contra Refl
      decEq (Net x t @{ald} ) (Net x' t' @{ald'}) | (No contra) = No $ \Refl => contra Refl
    decEq (Net x t) (Var y) = No $ \Refl impossible
    decEq (Var x) (Net y t) = No $ \Refl impossible
    decEq (Var x) (Var y) with (decEq x y)
      decEq (Var x) (Var x) | (Yes Refl) = Yes Refl
      decEq (Var x) (Var y) | (No contra) = No $ \Refl => contra Refl

  public export
  Eq SVObject where
    (==) a b = isYes $ decEq a b

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

  public export
  typeOf : (xs : SVObjList) -> Fin (length xs) -> SVObject
  typeOf (p::_ ) FZ     = p
  typeOf (_::ps) (FS i) = typeOf ps i

namespace IndexInObjects

  public export
  data IndexInObjects : SVObjList -> Type where
    Here : {x : SVObject} -> {xs : SVObjList} -> IndexInObjects (x :: xs)
    There : {x : SVObject} -> {xs : SVObjList} -> IndexInObjects xs -> IndexInObjects (x :: xs)

  public export
  DecEq (IndexInObjects objects) where
    decEq Here Here = Yes Refl
    decEq (Here) (There _) = No $ \Refl impossible
    decEq (There _) Here = No $ \Refl impossible
    decEq (There i) (There i') with (decEq i i')
      decEq (There i) (There i') | No p = No $ \Refl => p Refl
      decEq (There i) (There i) | Yes Refl = Yes Refl

  public export
  Eq (IndexInObjects objects) where
    (==) x y = isYes $ decEq x y

  public export
  data ListOfObjIndices : SVObjList -> Type where
    Nil  : {objects : _} -> ListOfObjIndices objects
    (::) : {objects : _} -> IndexInObjects objects -> ListOfObjIndices objects -> ListOfObjIndices objects

  public export
  DecEq (ListOfObjIndices objects) where
    decEq [] [] = Yes Refl
    decEq (a :: as) [] = No $ \Refl impossible
    decEq [] (b :: bs) = No $ \Refl impossible
    decEq (a :: as) (b :: bs) with (decEq a b)
      decEq (a :: as) (b :: bs) | No p = No $ \Refl => p Refl
      decEq (a :: as) (a :: bs) | Yes Refl with (decEq as bs)
        decEq (a :: as) (a :: bs) | Yes Refl | No p = No $ \Refl => p Refl
        decEq (a :: as) (a :: as) | Yes Refl | Yes Refl = Yes Refl

  public export
  Eq (ListOfObjIndices objects) where
    (==) x y = isYes $ decEq x y

  public export
  (++) : ListOfObjIndices objects -> ListOfObjIndices objects -> ListOfObjIndices objects
  (++) [] bs = bs
  (++) (a :: as) bs = a :: (as ++ bs)

  public export
  (.toList) : ListOfObjIndices objects -> List (IndexInObjects objects)
  (.toList) []        = []
  (.toList) (x :: xs) = x :: xs.toList

  public export
  (.fromList) : {objects : _} -> List (IndexInObjects objects) -> ListOfObjIndices objects
  (.fromList) []        = []
  (.fromList) (x :: xs) = x :: xs.fromList

  public export
  data TreeOfIndices : SVObjList -> Type where
    EmptyLeaf : TreeOfIndices objects
    Leaf : IndexInObjects objects -> TreeOfIndices objects
    Node2Degree : (left : TreeOfIndices objects) -> (right : TreeOfIndices objects) -> TreeOfIndices objects
    Node3Degree : (left : TreeOfIndices objects) ->
                  (middle : TreeOfIndices objects) ->
                  (right : TreeOfIndices objects) -> TreeOfIndices objects

  public export
  data AtIndexType : {objects : SVObjList} -> (i : IndexInObjects objects) -> (typeOfPort : SVType) -> Type where
    [search objects i]
    HereAtVar : {typeOfObject : SVType} -> {xs : SVObjList} -> AtIndexType {objects = (Var typeOfPort) :: xs} Here typeOfObject

    HereAtNet : {typeOfObject : SVType} -> {xs : SVObjList} -> (0 ald : AllowedNetData typeOfObject) =>
                AtIndexType {objects = (Net netTy @{ald} typeOfObject) :: xs} Here typeOfObject

    ThereAt : {typeOfPort : SVType} -> {x : SVObject} -> {xs : SVObjList} -> {i : IndexInObjects xs}
            -> (ati : AtIndexType {objects = xs} i typeOfPort) -> AtIndexType {objects = x :: xs} (There i) typeOfPort

  public export
  appendIfNew : {objects: _} -> ListOfObjIndices objects -> (i : IndexInObjects objects) -> ListOfObjIndices objects
  appendIfNew [] i = [i]
  appendIfNew (a :: as) i with (a == i)
    _ | True = a :: as
    _ | False = a :: appendIfNew as i

  public export
  union : {objects : _} -> ListOfObjIndices objects -> ListOfObjIndices objects -> ListOfObjIndices objects
  union x y = (x.toList `union` y.toList).fromList

  public export
  setEqual : ListOfObjIndices objects -> ListOfObjIndices objects -> Bool
  setEqual x y = ((x.toList `intersect` y.toList) == x.toList) && ((y.toList `intersect` x.toList) == y.toList)

  public export
  elem : IndexInObjects objects -> ListOfObjIndices objects -> Bool
  elem i [] = False
  elem i (x :: xs) with (decEq i x)
    _ | Yes _ = True
    _ | No _  = elem i xs

  public export
  data IsElemOf : (i : IndexInObjects objects) -> TreeOfIndices objects -> Type where
    InLeaf : IsElemOf i (Leaf i)
    OnTheLeft : IsElemOf i left -> IsElemOf i (Node2Degree leeft right)
    OnTheRight : IsElemOf i right -> IsElemOf i (Node2Degree left right)
    OnTheLeftMost : IsElemOf i left -> IsElemOf i (Node3Degree left middle right)
    OnTheMiddle : IsElemOf i middle -> IsElemOf i (Node3Degree left middle right)
    OnTheRightMost : IsElemOf i right -> IsElemOf i (Node3Degree left middle right)

  public export
  shortenIndexObjs : ListOfObjIndices (p :: ports) -> ListOfObjIndices ports
  shortenIndexObjs []        = []
  shortenIndexObjs (Here :: xs) = shortenIndexObjs xs
  shortenIndexObjs ((There x) :: xs) = x :: (shortenIndexObjs xs)

public export
isUnpacked' : SVType -> Bool
isUnpacked' (UnpackedArr _ _ _) = True
isUnpacked' _                   = False

public export
isUnpacked : SVObject -> Bool
isUnpacked (Net _ t) = isUnpacked' t
isUnpacked (Var   t) = isUnpacked' t
