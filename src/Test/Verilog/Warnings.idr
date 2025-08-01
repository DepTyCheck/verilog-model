module Test.Verilog.Warnings

import Data.List

import Test.Verilog.SVType

-- ||| Implicit cast situations:
-- |||
-- ||| module a(output logic [1:0] a1);
-- ||| endmodule: a
-- ||| 
-- ||| module b(input bit b1);
-- ||| endmodule: b
-- |||
-- ||| module c();
-- |||   a a_inst(w);      // #1  Cast logic [1:0] -> wire
-- |||   b b_inst(w);      // #1  Cast wire -> bit
-- ||| endmodule: c
-- |||
-- ||| module d(output byte d1, input reg d2);
-- |||   assign d1 = d2;   // #2  Cast reg -> byte
-- ||| endmodule: d
-- |||
-- ||| module e(input integer e1);
-- |||   b b_inst(e1);     // #3  Cast bit -> integer
-- ||| endmodule: e
-- |||
-- ||| module f(output int f1);
-- |||  a a_inst(f1);      // #4  Cast logic [1:0] -> int
-- ||| endmodule: f
-- |||
-- ||| module g();
-- |||   b b_inst(g2);     // #5  Cast wire -> bit
-- |||   a a_inst(g1);     // #6  Cast logic [1:0] -> wire
-- ||| endmodule: g
-- |||
-- ||| Implicit cast situations:                    | if unpacked then 0 casts else ...
-- ||| 1. Submodule source -> submodule sink        | 2 casts (source type -> default_net_type, default_net_type -> sink type)
-- ||| 2. Top source -> top sink                    | 1 cast  (source type -> sink type)
-- ||| 3. Top source -> submodule sink              | 1 cast  (source type -> sink type)
-- ||| 4. Submodule source -> top sink              | 1 cast  (source type -> sink type)
-- ||| 5. Unconnected submodule sink                | 1 cast  (source type -> default_net_type)
-- ||| 6. Unconnected submodule source              | 1 cast  (default_net_type -> sink type)  -- calculated in Pretty

export
Show State where
  show S2 = "2"
  show S4 = "4"

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

export
printStatesWarning : SVObject -> String -> SVObject -> String -> Maybe String
printStatesWarning op on np nn = do
  let olds = states op
  let news = states np
  case olds == news of
      True  => Nothing
      False => Just "// warning: implicit conversion changes possible bit states from \{show olds}-state to \{show news}-state"

parameters (showSVObj : SVObject -> (name : String) -> String)

  export
  printImplicitCast : SVObject -> String -> SVObject -> String -> List String
  printImplicitCast op on np nn = do
    let warnings = catMaybes [ printTruncationWarning op on np nn, printSignednessWarning op on np nn, printStatesWarning op on np nn ]
    case isNil warnings of
      True  => []
      False => warnings ++ [ "//   \{showSVObj op on} -> \{showSVObj np nn}" ]

  export
  printAllImplicitCasts : List SVObject -> List String -> List SVObject -> List String -> List String
  printAllImplicitCasts (p::ps) (n::ns) (p'::ps') (n'::ns') = do
    let curr = printImplicitCast p n p' n'
    let rest = printAllImplicitCasts ps ns ps' ns'
    if isNil curr || isNil rest then curr ++ rest else curr ++ ["//"] ++ rest
  printAllImplicitCasts _       _       _         _         = []
