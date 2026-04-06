module Test.VHDL.VHDLType

%default total

||| 5.2.2.2 Predefined enumeration types
||| IEEE 1076-2019
public export
data PredefinedEnumeration = CHARACTER | BIT | BOOLEAN | SEVERITY_LEVEL
-- There are also some other predefined enum types:
-- | RANGE_DIRECTION | FILE_OPEN_KIND | FILE_OPEN_STATUS | FILE_OPEN_STATE | FILE_ORIGIN_KIND


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
  -- Arrays 5.3.2 Array types
  -- Records 5.3.3 Record types
  ||| ΙΕΕΕ 1164−1993
  StdLogic : VHDLType
  -- STD_LOGIC_VECTOR
  -- resolved SIGNED and UNSIGNED from IEEE.NUMERIC_STD

public export
data VHDLPortMode = In | Out | InOut | Buffer | Linkage;

public export
Eq PredefinedEnumeration where
  (==) CHARACTER CHARACTER = True
  (==) BIT BIT = True
  (==) BOOLEAN BOOLEAN = True
  (==) SEVERITY_LEVEL SEVERITY_LEVEL = True
  (==) _ _ = False

public export
Eq VHDLType where
  (==) (Enum x) (Enum x') = x == x'
  (==) Integer' Integer' = True
  (==) Physical Physical = True
  (==) Real Real = True
  (==) StdLogic StdLogic = True
  (==) _ _ = False
  