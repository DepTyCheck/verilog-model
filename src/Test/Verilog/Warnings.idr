module Test.Verilog.Warnings

import Data.List

import Test.Verilog.SVType

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
