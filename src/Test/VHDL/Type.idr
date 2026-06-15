module Test.VHDL.Type

import public Data.Nat

%default total

||| 5.2.2.2 Predefined enumeration types
||| IEEE 1076-2019
public export
data PredefinedEnumeration = CHARACTER | BIT | BOOLEAN | SEVERITY_LEVEL
-- There are also some other predefined enum types:
-- | RANGE_DIRECTION | FILE_OPEN_KIND | FILE_OPEN_STATUS | FILE_OPEN_STATE | FILE_ORIGIN_KIND

public export
Eq PredefinedEnumeration where
  (==) CHARACTER CHARACTER = True
  (==) BIT BIT = True
  (==) BOOLEAN BOOLEAN = True
  (==) SEVERITY_LEVEL SEVERITY_LEVEL = True
  (==) _ _ = False


namespace Arrays

  ||| 5.3.2.3 Predefined array types
  ||| IEEE 1076-2019
  public export
  data PredefinedArrayTypes = BOOLEAN_VECTOR | BIT_VECTOR | INTEGER_VECTOR | REAL_VECTOR | TIME_VECTOR | STRING

  public export
  Eq PredefinedArrayTypes where
    (==) STRING STRING = True
    (==) BOOLEAN_VECTOR BOOLEAN_VECTOR = True
    (==) BIT_VECTOR BIT_VECTOR = True
    (==) INTEGER_VECTOR INTEGER_VECTOR = True
    (==) REAL_VECTOR REAL_VECTOR = True
    (==) TIME_VECTOR TIME_VECTOR = True
    (==) _ _ = False

  public export
  data ArrayDirection = Up | Down

  public export
  data IndexType = Natural | Positive

  public export
  data BoundsForPredefinedArrays : PredefinedArrayTypes -> IndexType -> Type where
    BBV  : BoundsForPredefinedArrays BOOLEAN_VECTOR Natural
    BBV' : BoundsForPredefinedArrays BIT_VECTOR Natural
    BIV  : BoundsForPredefinedArrays INTEGER_VECTOR Natural
    BRV  : BoundsForPredefinedArrays REAL_VECTOR Natural
    BTV  : BoundsForPredefinedArrays TIME_VECTOR Natural
    ||| 5.3.2.3 Predefined array types
    ||| type STRING is array (POSITIVE range <>) of CHARACTER;
    ||| IEEE 1076-2019
    BS   : BoundsForPredefinedArrays STRING Positive

  public export
  data CheckBounds : IndexType -> (start : Nat) -> (end : Nat) -> Type where
    MkNat : CheckBounds Natural s e
    MkPos : (s : Nat) -> (0 _ : IsSucc s) => (e : Nat) -> (0 _ : IsSucc e) => CheckBounds Positive s e

  public export
  record Dimension t where
    constructor MkDim
    start     : Nat
    end       : Nat
    direction : ArrayDirection
    {auto 0 bounds  : CheckBounds t start end}

  ||| 5.2 Scalar types
  ||| 5.2.1 General
  ||| The range L to R is called an ascending range; if L > R, then the range is a null range. The range L downto
  ||| R is called a descending range; if L < R, then the range is a null range. L is called the left bound of the range,
  ||| and R is called the right bound of the range
  ||| IEEE 1076-2019
  public export
  length : Dimension t -> Nat
  length (MkDim l r Up)   = if l > r then Z else S $ r `minus` l
  length (MkDim l r Down) = if l < r then Z else S $ l `minus` r

  public export %inline
  (.length) : Dimension t -> Nat
  (.length) = length

  ||| 9.2.3 Relational operators
  ||| For two one-dimensional array values, matching elements are those (if any) whose index values match in the
  ||| following sense: the left bounds of the index ranges are defined to match; if two elements match, the
  ||| elements immediately to their right are also defined to match.
  ||| IEEE 1076-2019
  |||
  ||| So the lengths must be equal, but the bounds and direction need not be
  public export
  dimsCompatible : Dimension t -> Dimension t' -> Bool
  dimsCompatible d d' = length d == length d'

-- ||| 5.3.2 Array types
-- ||| 5.3.2.1 General
-- ||| An array is characterised by:
-- |||   - its dimensionality (number of indices)
-- |||   - the type of each index
-- |||   - the range of each index
-- |||   - the element subtype
-- |||
-- ||| array_type_definition ::=
-- |||       unbounded_array_definition
-- |||     | constrained_array_definition
-- |||
-- ||| unbounded_array_definition   ::= array ( index_subtype_definition { , index_subtype_definition } )
-- |||                                     of element_subtype_indication
-- ||| constrained_array_definition ::= array index_constraint
-- |||                                     of element_subtype_indication
-- |||
-- ||| index_subtype_definition ::= type_mark range <>
-- ||| index_constraint         ::= ( discrete_range { , discrete_range } )
-- ||| IEEE 1076-2019

||| Currently only signal and variable types are implemented, and only these types can be declared as ports.
public export
data VHDLType : Type where
  Enum : PredefinedEnumeration -> VHDLType
  ||| 5.2.3.2 Predefined integer types
  ||| The only predefined integer type is the type INTEGER
  ||| IEEE 1076-2019
  Integer' : VHDLType
  ||| 5.2.4.2 Predefined physical types
  ||| The only predefined physical type is type TIME
  ||| IEEE 1076-2019
  Physical : VHDLType
  ||| 5.2.5.2 Predefined floating-point types
  ||| The only predefined floating-point type is the type REAL.
  ||| IEEE 1076-2019
  Real : VHDLType
  -- TODO: Declaration array without dims is allowed too e : out std_logic_vector;
  Array : (at : PredefinedArrayTypes) -> {0 _ : BoundsForPredefinedArrays at idt} -> Dimension idt -> VHDLType
  -- Records 5.3.3 Record types
  ||| ΙΕΕΕ 1164−1993
  StdLogic : VHDLType
  ||| ΙΕΕΕ 1164−1993
  StdLogicVector : Dimension Natural -> VHDLType
  -- resolved SIGNED and UNSIGNED from IEEE.NUMERIC_STD

public export
Eq VHDLType where
  (==) (Enum x) (Enum x') = x == x'
  (==) Integer' Integer' = True
  (==) Physical Physical = True
  (==) Real Real = True
  (==) StdLogic StdLogic = True
  (==) (Array t d) (Array t' d') = t == t' && dimsCompatible d d'
  (==) (StdLogicVector d) (StdLogicVector d') = dimsCompatible d d'
  (==) _ _ = False


public export
data VHDLPortMode = In | Out | InOut | Buffer | Linkage;

public export
Eq VHDLPortMode where
  (==) In In = True
  (==) Out Out = True
  (==) InOut InOut = True
  (==) Buffer Buffer = True
  (==) Linkage Linkage = True
  (==) _ _ = False
