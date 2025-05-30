module Test.Verilog.Warnings

import Test.Verilog.SVType

bitsCnt''' : IntegerVectorType -> Nat
bitsCnt''' Byte'     = 8
bitsCnt''' Shortint' = 16
bitsCnt''' Int'      = 32
bitsCnt''' Longint'  = 64
bitsCnt''' Integer'  = 32
bitsCnt''' Time'     = 64

bitsCnt'' : NonIntegerType -> Nat
bitsCnt'' Shortreal' = 32 
bitsCnt'' Real'      = 64 
bitsCnt'' Realtime'  = 64

bitsCnt' : SVType -> Nat
bitsCnt' (RVar x)            = bitsCnt'' x
bitsCnt' (SVar x)            = 1
bitsCnt' (VVar x)            = bitsCnt''' x
bitsCnt' (PackedArr   t s e) = S (max s e `minus` min s e) * bitsCnt' t
bitsCnt' (UnpackedArr t s e) = bitsCnt' t

export
bitsCnt : SVObject -> Nat
bitsCnt (Net _ t) = bitsCnt' t
bitsCnt (Var   t) = bitsCnt' t

export
printTruncationWarning : SVObject -> String -> SVObject -> String -> Maybe String
printTruncationWarning op on np nn = do
let oldb = bitsCnt op
let newb = bitsCnt np
case oldb == newb of
    True  => Nothing
    False => Just "// warning: implicit conversion of port connection \{specialWord oldb newb} from \{show oldb} to \{show newb} bits" where
    specialWord : (oldb : Nat) -> (newb : Nat) -> String
    specialWord o n =  if o > n then "truncates" else "expands"

isSigned'' : IntegerVectorType -> Bool
isSigned'' Byte'     = True
isSigned'' Shortint' = True
isSigned'' Int'      = True
isSigned'' Longint'  = True
isSigned'' Integer'  = True
isSigned'' Time'     = False

isSigned' : SVType -> Bool
isSigned' (RVar x)            = True
isSigned' (SVar x)            = False
isSigned' (VVar x)            = isSigned'' x
isSigned' (PackedArr   t _ _) = isSigned' t
isSigned' (UnpackedArr t _ _) = isSigned' t

||| 6.8 Variable Declarations
|||
||| The byte, shortint, int, integer, and longint types are signed types by default. Other net and
||| variable types can be explicitly declared as signed.
export
isSigned : SVObject -> Bool
isSigned (Net _ _) = False
isSigned (Var x)   = isSigned' x

export
printSignednessWarning : SVObject -> String -> SVObject -> String -> Maybe String
printSignednessWarning op on np nn = do
let olds = isSigned op
let news = isSigned np
case olds == news of
    True  => Nothing
    False => Just "// warning: implicit conversion changes signedness from \{specialWorld olds} to \{specialWorld news}" where
    specialWorld : Bool -> String
    specialWorld isSigned = if isSigned then "signed" else "unsigned"

states''' : IntegerScalarType -> Nat
states''' Bit'   = 2
states''' Logic' = 4
states''' Reg'   = 4

states'' : IntegerVectorType -> Nat
states'' Byte'     = 2 
states'' Shortint' = 2 
states'' Int'      = 2 
states'' Longint'  = 2 
states'' Integer'  = 4 
states'' Time'     = 4 

states' : SVType -> Nat
states' (RVar x)            = 4 
states' (SVar x)            = ?kjn_1 
states' (VVar x)            = states'' x 
states' (PackedArr   t s e) = states' t 
states' (UnpackedArr t s e) = states' t 

export
states : SVObject -> Nat
states (Net _ t) = 4
states (Var   t) = states' t

export
printStatesWarning : SVObject -> String -> SVObject -> String -> Maybe String
printStatesWarning op on np nn = do
let olds = states op
let news = states np
case olds == news of
    True  => Nothing
    False => Just "// warning: implicit conversion changes possible bit states from \{show olds}-state to \{show news}-state"