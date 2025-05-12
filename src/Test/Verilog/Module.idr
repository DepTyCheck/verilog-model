module Test.Verilog.Module

import Data.Fuel
import Data.Vect
import public Data.Fin

import Test.DepTyCheck.Gen
import Test.DepTyCheck.Gen.Coverage

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

export
DecEq SVBasic where
  decEq Logic' Logic' = Yes Refl
  decEq Logic' Wire' = No $ \Refl impossible
  decEq Logic' Uwire' = No $ \Refl impossible
  decEq Logic' Int' = No $ \Refl impossible
  decEq Logic' Integer' = No $ \Refl impossible
  decEq Logic' Bit' = No $ \Refl impossible
  decEq Logic' Real' = No $ \Refl impossible
  decEq Wire' Logic' = No $ \Refl impossible
  decEq Wire' Wire' = Yes Refl
  decEq Wire' Uwire' = No $ \Refl impossible
  decEq Wire' Int' = No $ \Refl impossible
  decEq Wire' Integer' = No $ \Refl impossible
  decEq Wire' Bit' = No $ \Refl impossible
  decEq Wire' Real' = No $ \Refl impossible
  decEq Uwire' Logic' = No $ \Refl impossible
  decEq Uwire' Wire' = No $ \Refl impossible
  decEq Uwire' Uwire' = Yes Refl
  decEq Uwire' Int' = No $ \Refl impossible
  decEq Uwire' Integer' = No $ \Refl impossible
  decEq Uwire' Bit' = No $ \Refl impossible
  decEq Uwire' Real' = No $ \Refl impossible
  decEq Int' Logic' = No $ \Refl impossible
  decEq Int' Wire' = No $ \Refl impossible
  decEq Int' Uwire' = No $ \Refl impossible
  decEq Int' Int' = Yes Refl
  decEq Int' Integer' = No $ \Refl impossible
  decEq Int' Bit' = No $ \Refl impossible
  decEq Int' Real' = No $ \Refl impossible
  decEq Integer' Logic' = No $ \Refl impossible
  decEq Integer' Wire' = No $ \Refl impossible
  decEq Integer' Uwire' = No $ \Refl impossible
  decEq Integer' Int' = No $ \Refl impossible
  decEq Integer' Integer' = Yes Refl
  decEq Integer' Bit' = No $ \Refl impossible
  decEq Integer' Real' = No $ \Refl impossible
  decEq Bit' Logic' = No $ \Refl impossible
  decEq Bit' Wire' = No $ \Refl impossible
  decEq Bit' Uwire' = No $ \Refl impossible
  decEq Bit' Int' = No $ \Refl impossible
  decEq Bit' Integer' = No $ \Refl impossible
  decEq Bit' Bit' = Yes Refl
  decEq Bit' Real' = No $ \Refl impossible
  decEq Real' Logic' = No $ \Refl impossible
  decEq Real' Wire' = No $ \Refl impossible
  decEq Real' Uwire' = No $ \Refl impossible
  decEq Real' Int' = No $ \Refl impossible
  decEq Real' Integer' = No $ \Refl impossible
  decEq Real' Bit' = No $ \Refl impossible
  decEq Real' Real' = Yes Refl

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

export
Injective Var where
  injective Refl = Refl

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

export
DecEq (AllowedInPackedArr t)

export
DecEq (SVArray t start end)

export
DecEq SVType where
  decEq (Arr (Unpacked t s e)) (Arr (Unpacked t' s' e')) with (decEq t t')
    decEq (Arr (Unpacked t s e)) (Arr (Unpacked t' s' e')) | No p = No $ \Refl => p Refl
    decEq (Arr (Unpacked t s e)) (Arr (Unpacked t s' e')) | Yes Refl with (decEq s s')
      decEq (Arr (Unpacked t s e)) (Arr (Unpacked t s' e')) | Yes Refl | No p = No $ \Refl => p Refl
      decEq (Arr (Unpacked t s e)) (Arr (Unpacked t s e')) | Yes Refl | Yes Refl with (decEq e e')
        decEq (Arr (Unpacked t s e)) (Arr (Unpacked t s e')) | Yes Refl | Yes Refl | No p = No $ \Refl => p Refl
        decEq (Arr (Unpacked t s e)) (Arr (Unpacked t s e)) | Yes Refl | Yes Refl | Yes Refl = Yes Refl
  decEq (Arr (Unpacked t s e)) (Arr (Packed t' s' e' @{k'})) = No $ \Refl impossible
  decEq (Arr (Packed t s e @{k})) (Arr (Unpacked t' s' e')) = No $ \Refl impossible
  decEq (Arr (Packed t s e @{k})) (Arr (Packed t' s' e' @{k'})) with (decEq t t')
    decEq (Arr (Packed t s e @{k})) (Arr (Packed t' s' e' @{k'})) | No p = No $ \Refl => p Refl
    decEq (Arr (Packed t s e @{k})) (Arr (Packed t s' e' @{k'})) | Yes Refl with (decEq s s')
      decEq (Arr (Packed t s e @{k})) (Arr (Packed t s' e' @{k'})) | Yes Refl | No p = No $ \Refl => p Refl
      decEq (Arr (Packed t s e @{k})) (Arr (Packed t s e' @{k'})) | Yes Refl | Yes Refl with (decEq e e')
        decEq (Arr (Packed t s e @{k})) (Arr (Packed t s e' @{k'})) | Yes Refl | Yes Refl | No p = No $ \Refl => p Refl
        decEq (Arr (Packed t s e @{k})) (Arr (Packed t s e @{k'})) | Yes Refl | Yes Refl | Yes Refl with (decEq k k')
          decEq (Arr (Packed t s e @{k})) (Arr (Packed t s e @{k'})) | Yes Refl | Yes Refl | Yes Refl | No p = No $ \Refl => p Refl
          decEq (Arr (Packed t s e @{k})) (Arr (Packed t s e @{k})) | Yes Refl | Yes Refl | Yes Refl | Yes Refl = Yes Refl
  decEq (Arr x) (Var y) = No $ \Refl impossible
  decEq (Var x) (Arr y) = No $ \Refl impossible
  decEq (Var x) (Var y) = decEqCong (decEq x y)

DecEq (AllowedInPackedArr t) where
  decEq B B = Yes Refl
  decEq L L = Yes Refl
  decEq P P = Yes Refl

DecEq (SVArray t start end) where
  decEq (Unpacked t start end) (Unpacked t start end) = Yes Refl
  decEq (Unpacked _ _ _) (Packed _ _ _) = No $ \Refl impossible
  decEq (Packed t start end @{k}) (Unpacked t start end) = No $ \Refl impossible
  decEq (Packed t start end @{k}) (Packed t start end @{k'}) with (decEq k k')
    decEq (Packed t start end @{k}) (Packed t start end @{k'}) | No p = No $ \Refl => p Refl
    decEq (Packed t start end @{k}) (Packed t start end @{k})  | Yes Refl = Yes Refl

namespace Ports

  public export
  data PortsList = Nil | (::) SVType PortsList

  export
  DecEq PortsList where
    decEq [] [] = Yes Refl
    decEq [] (_ :: _) = No $ \Refl impossible
    decEq (x :: z) [] = No $ \Refl impossible
    decEq (x :: xs) (x' :: xs'') with (decEq x x')
      decEq (x :: xs) (x' :: xs'') | No p = No $ \Refl => p Refl
      decEq (x :: xs) (x :: xs'') | Yes Refl with (decEq xs xs'')
        decEq (x :: xs) (x :: xs'') | Yes Refl | No p = No $ \Refl => p Refl
        decEq (x :: xs) (x :: xs) | Yes Refl | Yes Refl = Yes Refl

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

  public export
  typeOf : (xs : PortsList) -> Fin (length xs) -> SVType
  typeOf (p::_ ) FZ     = p
  typeOf (_::ps) (FS i) = typeOf ps i

namespace IndexInPorts

  public export
  data IndexInPorts : SVType -> PortsList -> Type where
    Here : {x : SVType} -> {xs : PortsList} -> IndexInPorts x (x :: xs)
    There : {y : SVType} -> {xs : PortsList} -> {x : SVType} -> IndexInPorts y xs -> IndexInPorts y (x :: xs)

  export
  DecEq (IndexInPorts x ports) where
    decEq Here Here = Yes Refl
    decEq (Here {xs = xs} {x = x}) (There {x} {xs} {y = x} y) = No $ \Refl impossible
    decEq (There _) Here = No $ \Refl impossible
    decEq (There {y} {xs} {x} i) (There {y} {xs} {x} z) = ?k_3

  public export
  data ListOfPortsIndices : PortsList -> Type where
    Nil  : {ports : _} -> ListOfPortsIndices ports
    (::) : {x : _} -> {ports : _} -> IndexInPorts x ports -> ListOfPortsIndices ports -> ListOfPortsIndices ports

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
  (.length) []      = 0
  (.length) (x::xs) = S xs.length

  public export
  index : (fs : FinsList s) -> Fin fs.length -> Fin s
  index (f::_ ) FZ     = f
  index (_::fs) (FS i) = index fs i

  public export
  fromVect : Vect l (Fin sk) -> FinsList sk
  fromVect []      = []
  fromVect (x::xs) = x :: fromVect xs

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

  ||| Returns the size of packed array.
  ||| But the actual number of bits that a type stores may be different (Var SVBasic represents types of different sizes)
  public export
  packedSize : SVType -> Nat
  packedSize (Var _)                = 1
  packedSize (Arr $ Unpacked t _ _) = packedSize t
  packedSize (Arr $ Packed   t s e) = S (max s e `minus` min s e) * packedSize t

  public export
  data EqSuperBasic : SVType -> SVType -> Type where
    EqBasicV : EqSVBasic    t t' -> EqSuperBasic (Var t)                    (Var t')
    EqBasicP : EqSuperBasic t t' -> EqSuperBasic (Arr $ Packed   t {} @{_}) (Arr $ Packed   t' {} @{_})
    EqBasicU : EqSuperBasic t t' -> EqSuperBasic (Arr $ Unpacked t {})      (Arr $ Unpacked t' {})

  public export
  data VarOrPacked : SVType -> Type where
    V : VarOrPacked (Var _)
    P : VarOrPacked (Arr (Packed {} @{_}))

  public export
  data EqUnpackedArrSig : SVType -> SVType -> Type where
    Other  : VarOrPacked t -> VarOrPacked t' -> EqUnpackedArrSig t t'
    EqUArr : EqUnpackedArrSig t t' -> EqNat (max s e + min s' e') (max s' e' + min s e) ->
      EqUnpackedArrSig (Arr $ Unpacked t s e) (Arr $ Unpacked t' s' e')

  public export
  data CanConnect : SVType -> SVType -> Type where
    CCVarOrPacked : VarOrPacked p1 -> VarOrPacked p2 -> CanConnect p1 p2
    ||| 6.22.2 Equivalent types
    ||| d) Unpacked fixed-size array types are equivalent if they have equivalent element types and equal size.
    |||
    ||| IEEE 1800 - 2023
    CCUnpackedUnpacked : EqSuperBasic t t' -> EqNat (packedSize t) (packedSize t') ->
      EqUnpackedArrSig (Arr $ Unpacked t s e) (Arr $ Unpacked t' s' e') -> CanConnect (Arr $ Unpacked t s e) (Arr $ Unpacked t' s' e')

  ||| The list of sources may be empty (Nil). In this case, either an implicit net is declared or an external net declaration must exist
  |||
  ||| > If an identifier is used in a port expression declaration,
  ||| then an implicit net of default net type shall be assumed, with the vector width of the port expression declaration.
  |||
  ||| IEEE 1800-2023
  public export
  data SourceForSink : (srcs : PortsList) -> (sink : SVType) -> Type where
    NoSource  : SourceForSink srcs sink
    HasSource : (srcIdx : Fin $ length srcs) -> CanConnect (typeOf srcs srcIdx) sink -> SourceForSink srcs sink

namespace ConnsList

  public export
  data NotEqFin : Fin n -> Fin n -> Type where
    ZS  : NotEqFin FZ (FS i)
    SZ  : NotEqFin (FS i) FZ
    Rec : NotEqFin x y -> NotEqFin (FS x) (FS y)

  public export
  data Connections : (srcs, sinks : PortsList) -> (isUnique : Bool) -> Type

  public export
  data NoSourceConns : SourceForSink srcs sink' -> Connections srcs sinks isUnique -> Type

  ||| Each output maybe has connection from some input.
  ||| If isUnique then each input can go to one output. Otherwise each input can go to several outputs
  public export
  data Connections : (srcs, sinks : PortsList) -> (isUnique : Bool) -> Type where
    Empty : Connections srcs [] u
    Cons  : (sfs : SourceForSink srcs sink) -> (rest : Connections srcs sinks u) -> NoSourceConns sfs rest -> Connections srcs (sink :: sinks) u

  ||| List of source indexes
  public export
  consToFins : Connections srcs sinks u -> FinsList (srcs.length)
  consToFins Empty                              = []
  consToFins (Cons NoSource             rest _) = consToFins rest
  consToFins (Cons (HasSource srcIdx _) rest _) = srcIdx :: consToFins rest

  public export
  data FinNotIn : FinsList srcs -> Fin srcs -> Type where
    FNIEmpty : FinNotIn [] f
    FNICons  : {x, f : Fin srcs} -> (0 _ : NotEqFin x f) -> (fni: FinNotIn xs f) -> FinNotIn (x :: xs) f

  ||| If Connections are indexed as Unique, then source indexes must not repeat
  public export
  data NoSourceConns : (sfs : SourceForSink srcs sink') -> (conns : Connections srcs sinks isUnique) -> Type where
    NotUnique : {conns : Connections srcs sinks False} -> NoSourceConns sfs conns
    ConsNoS   : {conns : Connections srcs sinks True } -> NoSourceConns NoSource conns
    ConsHasS  : {conns : Connections srcs sinks True } -> FinNotIn (consToFins conns) f -> NoSourceConns (HasSource f cc) conns

public export
data Modules : ModuleSigsList -> Type where

  End : Modules ms

  ||| A module containing only submodules and connections.
  NewCompositeModule :
    (m : ModuleSig) ->
    (subMs : FinsList ms.length) ->
    -- Remember: Do not change the concatenation order of the port lists, the printer and the assigns depend on it
    (sssi : Connections (m.inputs ++ allOutputs {ms} subMs) (allInputs {ms} subMs) False) ->
    (ssto : Connections (m.inputs ++ allOutputs {ms} subMs) (m.outputs)            True ) ->
    (cont : Modules (m::ms)) ->
    Modules ms


export
genNotEqFin : Fuel -> {n : Nat} -> (a, b : Fin n) -> Gen MaybeEmpty $ NotEqFin a b
export
genSourceForSink : Fuel -> (srcs : PortsList) -> (sink' : SVType) -> Gen MaybeEmpty $ SourceForSink srcs sink'

genFinNotIn : Fuel -> {srcs : Nat} -> (fins : FinsList srcs) -> (fin : Fin srcs) -> Gen MaybeEmpty $ FinNotIn fins fin
genFinNotIn x []        fin = pure FNIEmpty
genFinNotIn x (f :: fs) fin = do
  rest <- genFinNotIn x fs fin
  ne <- genNotEqFin x f fin
  pure $ FNICons ne rest

genNoSourceConns : Fuel -> {isUnique : Bool} -> {srcs : PortsList} ->
                   (sfs : SourceForSink srcs sink) -> (conns : Connections srcs sinks isUnique) -> Gen MaybeEmpty $ NoSourceConns sfs conns
genNoSourceConns x {isUnique = False} sfs conns = pure NotUnique
genNoSourceConns x {isUnique = True} NoSource conns = pure ConsNoS
genNoSourceConns x {isUnique = True} (HasSource srcIdx y) conns = do
  fni <- genFinNotIn x (consToFins conns) srcIdx
  pure $ ConsHasS fni

export
genConnections : Fuel -> (srcs : PortsList) -> (sinks : PortsList) -> (isUnique : Bool) -> Gen MaybeEmpty $ Connections srcs sinks isUnique
genConnections x srcs [] u        = pure Empty
genConnections x srcs (y :: ys) u = do
  sfs <- genSourceForSink x srcs y
  rest <- genConnections x srcs ys u
  nsc <- genNoSourceConns x sfs rest
  pure $ Cons sfs rest nsc

export
genModules : Fuel -> (ms : ModuleSigsList) ->
  (Fuel -> (srcs : PortsList) -> (sink' : SVType) -> Gen MaybeEmpty $ SourceForSink srcs sink') =>
  (Fuel -> (srcs' : PortsList) -> (sinks' : PortsList) -> (isUnique' : Bool) -> Gen MaybeEmpty $ Connections srcs' sinks' isUnique') =>
  Gen MaybeEmpty $ Modules ms
