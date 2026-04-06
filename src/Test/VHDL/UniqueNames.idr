module Test.VHDL.UniqueNames

import Data.Fuel

import Test.Common.UniqueNames

import Test.DepTyCheck.Gen

%default total

export
VHDLKeywords : SVect ?
VHDLKeywords = [
  -- 15.10 Reserved words
  "abs", "fairness", "nand", "select", "access", "file", "new", "sequence", "after", "for", "next", "severity", "alias", "force", "nor", "signal",
  "all", "function", "not", "shared", "and", "null", "sla", "architecture", "generate", "sll", "array", "generic", "of", "sra", "assert", "group",
  "on", "srl", "assume", "guarded", "open", "strong", "attribute", "or", "subtype", "if", "others", "begin", "impure", "out", "then", "block", "in",
  "to", "body", "inertial", "package", "transport", "buffer", "inout", "parameter", "type", "bus", "is", "port", "postponed", "unaffected", "case",
  "label", "procedure", "units", "component", "library", "process", "until", "configuration", "linkage", "property", "use", "constant", "context",
  "literal", "protected", "loop", "private", "variable", "cover", "pure", "view", "map", "vpkg", "default", "mod", "range", "vmode", "disconnect",
  "downto", "record", "vprop", "register", "vunit", "reject", "else", "release", "wait", "elsif", "rem", "when", "end", "report", "while", "entity",
  "exit", "restrict", "with", "return", "rol", "xnor", "ror", "xor"
]

export
genOneName : {l : Nat} -> Fuel -> (names : SVect l) -> (un : UniqNames l names) ->
              Gen MaybeEmpty (out : String ** UniqNames (S l) (out :: names))
genOneName = genOneUniqueName VHDLKeywords

export
genManyNames : {l : Nat} -> Fuel -> (n : Nat) -> (names : SVect l) -> (un : UniqNames l names) ->
                 Gen MaybeEmpty (newNames : SVect n ** UniqNames (n + l) (newNames ++ names))
genManyNames = genNUniqueNames VHDLKeywords
