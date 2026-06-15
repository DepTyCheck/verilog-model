module Test.VHDL.Literal

import Data.Vect

import public Test.VHDL.Type

%default total

namespace NatsListNE

  public export
  data NatsListNE : Type where
    One  : Nat -> NatsListNE
    More : Nat -> NatsListNE -> NatsListNE

  %name NatsListNE ns

  export
  toList : NatsListNE -> List Nat
  toList (One n)     = [ n ]
  toList (More n ns) = n :: toList ns

public export
data NumBase = Binary | Octal | Hexadecimal

namespace Bool'

  public export
  data BoolValue = False | True

namespace Bin

  public export
  data BinValue = Zero
                | One

namespace Oct

  public export
  data OctValue = Zero
                | One
                | Two
                | Three
                | Four
                | Five
                | Six
                | Seven

namespace Hex

  public export
  data HexValue = Zero
                | One
                | Two
                | Three
                | Four
                | Five
                | Six
                | Seven
                | Eight
                | Nine
                | A
                | B
                | C
                | D
                | E
                | F

namespace BaseValuesList

  public export
  data BaseValue : NumBase -> Type where
    MkBin : BinValue -> BaseValue Binary
    MkOct : OctValue -> BaseValue Octal
    MkHex : HexValue -> BaseValue Hexadecimal

  public export
  data BaseValuesList : NumBase -> Type where
    One  : BaseValue b -> BaseValuesList b
    More : BaseValue b -> BaseValuesList b -> BaseValuesList b

  %name BaseValuesList bs

  export
  toList : BaseValuesList b -> List $ BaseValue b
  toList (One x)    = [ x ]
  toList (More x xs) = x :: toList xs

namespace CoreTypes

  ||| 5.2.3 Integer types
  ||| 5.2.3.1 General
  ||| Integer literals are the literals of an anonymous predefined type that is called universal_integer in this standard.
  ||| Other integer types have no literals.
  ||| However, for each integer type there exists an implicit conversion that converts a value of type universal_integer
  ||| into the corresponding value (if any) of the integer type.
  |||
  ||| IEEE 1076-2019
  public export
  data UniversalInteger : Type where
    MkInt  : NatsListNE -> UniversalInteger
    MkBInt : (n : NumBase) -> BaseValuesList n -> UniversalInteger

  public export
  data UniversalReal : Type where
    MkReal  : NatsListNE -> NatsListNE -> UniversalReal
    MkBReal : (n : NumBase) -> BaseValuesList n -> BaseValuesList n -> UniversalReal

namespace Enums

  ||| 16.3 Package STANDARD
  ||| type SEVERITY_LEVEL is (NOTE, WARNING, ERROR, FAILURE);
  ||| IEEE 1076-2019
  public export
  data SeverityLevelValue = NOTE | WARNING | ERROR | FAILURE

namespace Time
  ||| 16.3 Package STANDARD
  ||| type TIME is range implementation_defined
  |||   units
  |||     fs; -- femtosecond
  |||     ps = 1000 fs; -- picosecond
  |||     ns = 1000 ps; -- nanosecond
  |||     us = 1000 ns; -- microsecond
  |||     ms = 1000 us; -- millisecond
  |||     sec = 1000 ms; -- second
  |||     min = 60 sec; -- minute
  |||     hr = 60 min; -- hour
  |||   end units;
  ||| IEEE 1076-2019
  public export
  data TimeUnits = Fs | Ps | Ns | Us | Ms | Sec | Min | Hr

  ||| 5.2.4 Physical types
  ||| 5.2.4.1 General
  ||| There is a position number corresponding to each value of a physical type.
  ||| The position number of the value corresponding to a unit name is the number of primary units represented by that unit name.
  ||| The position number of the value corresponding to a physical literal with an abstract literal part is
  ||| the largest integer that is not greater than the product of the value of
  ||| the abstract literal and the position number of the accompanying unit name.
  |||
  ||| 5.2.4.2 Predefined physical types
  ||| The range of TIME is implementation dependent and shall include the range -(2^63) to (2^63)-1.
  |||
  ||| IEEE 1076-2019
  |||
  ||| So possible max values for units:
  ||| fs = 9_223_372_036_854_775_807 ≈ 9.22 * 10^18
  ||| ps ≈ 9.22 * 10^15
  ||| ns ≈ 9.22 * 10^12
  ||| us ≈ 9.22 * 10^9
  ||| ms ≈ 9.22 * 10^6
  ||| sec ≈ 9.22 * 10^3
  ||| min = 153
  ||| hr = 2
  public export
  data TimeValue : TimeUnits -> Type where
    FU : UniversalReal -> TimeValue Fs
    FI : UniversalInteger -> TimeValue Fs
    PU : UniversalReal -> TimeValue Ps
    PI : UniversalInteger -> TimeValue Ps
    NU : UniversalReal -> TimeValue Ns
    NI : UniversalInteger -> TimeValue Ns
    UU : UniversalReal -> TimeValue Us
    UI : UniversalInteger -> TimeValue Us
    MU : UniversalReal -> TimeValue Ms
    MI : UniversalInteger -> TimeValue Ms
    SN : (n : Nat) -> LT n 1000 -> TimeValue Sec
    MN : (n : Nat) -> LT n 153 -> TimeValue Min
    HN : (n : Nat) -> LT n 2 -> TimeValue Hr

  public export
  data TimeLiteral : Type where
    TL : (u : TimeUnits) -> TimeValue u -> TimeLiteral

namespace CharVect

  public export
  data CharVect : Nat -> Type where
    Nil  : CharVect 0
    (::) : Char -> CharVect n -> CharVect (S n)

  public export
  toVect : CharVect n -> Vect n Char
  toVect []        = []
  toVect (x :: xs) = x :: toVect xs

namespace BinVect

  public export
  data BinVect : Nat -> Type where
    Nil  : BinVect 0
    (::) : BinValue -> BinVect n -> BinVect (S n)

  public export
  toVect : BinVect n -> Vect n BinValue
  toVect []        = []
  toVect (x :: xs) = x :: toVect xs

namespace BoolVect

  public export
  data BoolVect : Nat -> Type where
    Nil  : BoolVect 0
    (::) : BoolValue -> BoolVect n -> BoolVect (S n)

  public export
  toVect : BoolVect n -> Vect n BoolValue
  toVect []        = []
  toVect (x :: xs) = x :: toVect xs

namespace IntVect

  public export
  data IntVect : Nat -> Type where
    Nil  : IntVect 0
    (::) : UniversalInteger -> IntVect n -> IntVect (S n)

  public export
  toVect : IntVect n -> Vect n UniversalInteger
  toVect []        = []
  toVect (x :: xs) = x :: toVect xs

namespace RealVect

  public export
  data RealVect : Nat -> Type where
    Nil  : RealVect 0
    (::) : UniversalReal -> RealVect n -> RealVect (S n)

  public export
  toVect : RealVect n -> Vect n UniversalReal
  toVect []        = []
  toVect (x :: xs) = x :: toVect xs

namespace TimeVect

  public export
  data TimeVect : Nat -> Type where
    Nil  : TimeVect 0
    (::) : TimeLiteral -> TimeVect n -> TimeVect (S n)

  public export
  toVect : TimeVect n -> Vect n TimeLiteral
  toVect []        = []
  toVect (x :: xs) = x :: toVect xs

namespace IEEE_1164

  ||| IEEE 1164-1993
  public export
  data LogicValue = U  -- Uninitialized
                  | X  -- Forcing Unknown
                  | F0 -- Forcing 0
                  | F1 -- Forcing 1
                  | Z' -- High Impedance
                  | W  -- Weak Unknown
                  | L  -- Weak 0
                  | H  -- Weak 1
                  | DC -- Don't care

  public export
  data LogicVect : Nat -> Type where
    Nil  : LogicVect Z
    (::) : LogicValue -> LogicVect n -> LogicVect (S n)

  public export
  toVect : LogicVect n -> Vect n LogicValue
  toVect []        = []
  toVect (x :: xs) = x :: toVect xs

||| 15.5.2 Decimal literals
||| A decimal literal is an abstract literal expressed in the conventional decimal notation (that is, the base is implicitly ten).
||| decimal_literal ::= integer [ . integer ] [ exponent ]
||| integer ::= digit { [ underline ] digit }
||| exponent ::= E [ + ] integer | E - integer
|||
||| IEEE 1076-2019
public export
data VHDLLiteral : VHDLType -> Type where
  -- ENUMS
  MkChar      : Char -> VHDLLiteral $ Enum CHARACTER
  ||| 16.3 Package STANDARD
  ||| type BOOLEAN is (FALSE, TRUE);
  ||| IEEE 1076-2019
  MkBIT       : BinValue -> VHDLLiteral $ Enum BIT
  ||| 16.3 Package STANDARD
  ||| type BIT is ('0', '1');
  ||| IEEE 1076-2019
  MkBool      : BoolValue -> VHDLLiteral $ Enum BOOLEAN
  MkSevLevel  : SeverityLevelValue -> VHDLLiteral $ Enum SEVERITY_LEVEL
  -- INTEGER
  MkInt       : UniversalInteger -> VHDLLiteral Integer'
  -- TIME
  MkPhys      : TimeLiteral -> VHDLLiteral Physical
  -- REAL
  MkReal      : UniversalReal -> VHDLLiteral Real
  -- ARRAYS
  MkString    : CharVect dims.length -> VHDLLiteral $ Array STRING dims
  MkBoolVect  : BoolVect dims.length -> VHDLLiteral $ Array BOOLEAN_VECTOR dims
  MkBitVect   : BinVect dims.length -> VHDLLiteral $ Array BIT_VECTOR dims
  MkIntVect   : IntVect dims.length -> VHDLLiteral $ Array INTEGER_VECTOR dims
  MkRealVect  : RealVect dims.length -> VHDLLiteral $ Array REAL_VECTOR dims
  MkTimeVect  : TimeVect dims.length -> VHDLLiteral $ Array TIME_VECTOR dims
  -- LOGICS
  MkLogic     : LogicValue -> VHDLLiteral StdLogic
  MkLogicVect : LogicVect dims.length -> VHDLLiteral $ StdLogicVector dims
