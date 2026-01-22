module Test.Verilog.UniqueNames

import Data.List.Extra
import Data.String

import Data.Fuel

import Test.DepTyCheck.Gen
import System.Random.Pure.StdGen


%default total


public export
data SVect : (len : Nat) -> Type where
  ||| Empty vector
  Nil  : SVect Z
  ||| A non-empty vector of length `S len`, consisting of a head element and
  ||| the rest of the list, of length `len`.
  (::) : (x : String) -> (xs : SVect len) -> SVect (S len)

(++) : SVect a -> SVect b -> SVect (a + b)
(++) [] xs = xs
(++) (x :: xs) ys = x :: (xs ++ ys)

(.length) : SVect l -> Nat
(.length) [] = Z
(.length) (x :: xs) = S xs.length

export
toVect : SVect l -> Vect l String
toVect [] = []
toVect (x :: xs) = x :: toVect xs

data UniqNames : (l : Nat) -> SVect l -> Type
data NameNotIn : (l : Nat) -> (names : SVect l) -> (name : String) -> Type

public export
data UniqNames : (l : Nat) -> SVect l -> Type where
  Empty : UniqNames 0 []
  Cons : {l : Nat} -> (names : SVect l) -> (name: String) -> UniqNames l names -> NameNotIn l names name -> UniqNames (S l) (name :: names)

public export
data NameNotIn : (l : Nat) -> (names : SVect l) -> (name : String) -> Type where
  NNPEmpty : NameNotIn 0 [] s
  NNPCons : (0 _ : So $ x /= name) -> (npi: NameNotIn l xs name) -> NameNotIn (S l) (x :: xs) name

VerilogKeywords : SVect ?
VerilogKeywords = [
  "accept_on", "alias", "always", "always_comb", "always_ff", "always_latch", "and", "assert", "assign", "assume", "automatic", "before", "begin",
  "bind", "bins", "binsof", "bit", "break", "buf", "bufif0", "bufif1", "byte", "case", "casex", "casez", "cell", "chandle", "checker", "class",
  "clocking", "cmos", "config", "const", "constraint", "context", "continue", "cover", "covergroup", "coverpoint", "cross", "deassign", "default",
  "defparam", "design", "disable", "dist", "do", "edge", "else", "end", "endcase", "endchecker", "endclass", "endclocking","endconfig", "endfunction",
  "endgenerate", "endgroup", "endinterface", "endmodule", "endpackage", "endprimitive", "endprogram", "endproperty", "endspecify", "endsequence",
  "endtable", "endtask", "enum", "event", "eventually", "expect", "export", "extends", "extern", "final", "first_match", "for", "force", "foreach",
  "forever", "fork", "forkjoin", "function", "generate", "genvar", "global", "highz0", "highz1", "if", "iff", "ifnone", "ignore_bins", "illegal_bins",
  "implements", "implies", "import", "incdir", "include", "initial", "inout", "input", "inside", "instance", "int", "integer", "interconnect",
  "interface", "intersect", "join", "join_any", "join_none", "large", "let", "liblist", "library", "local", "localparam", "logic", "longint",
  "macromodule", "matches", "medium", "modport", "module", "nand", "negedge", "nettype", "new", "nexttime", "nmos", "nor", "noshowcancelled", "not",
  "notif0", "notif1", "null", "or", "output", "package", "packed", "parameter", "pmos", "posedge", "primitive", "priority", "program", "property",
  "protected", "pull0", "pull1", "pulldown", "pullup", "pulsestyle_ondetect", "pulsestyle_onevent", "pure", "rand", "randc", "randcase", "randsequence",
  "rcmos", "real", "realtime", "ref", "reg", "reject_on", "release", "repeat", "restrict", "return", "rnmos", "rpmos", "rtran", "rtranif0", "rtranif1",
  "s_always", "s_eventually", "s_nexttime", "s_until", "s_until_with", "scalared", "sequence", "shortint", "shortreal", "showcancelled", "signed",
  "small", "soft", "solve", "specify", "specparam", "static", "string", "strong", "strong0", "strong1", "struct", "super", "supply0", "supply1",
  "sync_accept_on", "sync_reject_on", "table", "tagged", "task", "this", "throughout", "time", "timeprecision", "timeunit", "tran", "tranif0",
  "tranif1", "tri", "tri0", "tri1", "triand", "trior", "trireg", "type", "typedef", "union", "unique", "unique0", "unsigned", "until", "until_with",
  "untyped", "use", "uwire", "var", "vectored", "virtual", "void", "wait", "wait_order", "wand", "weak", "weak0", "weak1", "while", "wildcard", "wire",
  "with", "within", "wor", "xnor", "xor"
]

public export
data NameIsNewAndNonKeyword : (keywords : SVect lk) -> (names: SVect l) -> (un: UniqNames l names) -> (name : String) -> Type where
  NINANK : NameNotIn l names name -> NameNotIn lk keywords name -> NameIsNewAndNonKeyword keywords names un name

export
rawNewName' : Fuel ->
              (Fuel -> Gen MaybeEmpty String) =>
              {l : Nat} -> {lk : Nat} ->
              (keywords : SVect lk) ->
              (names: SVect l) ->
              (un: UniqNames l names) ->
              Gen MaybeEmpty (s ** NameIsNewAndNonKeyword keywords names un s)

export
rawNewName : Fuel -> (Fuel -> Gen MaybeEmpty String) =>
             {l : Nat} -> (names: SVect l) -> (un: UniqNames l names) -> Gen MaybeEmpty (s ** NameNotIn l names s)
rawNewName f @{g} names un = do
  (s ** NINANK a b) <- rawNewName' f @{g} VerilogKeywords names un
  pure (s ** a)

namesGen : Gen0 String
namesGen = pack <$> listOf {length = choose (1,10)} (choose ('a', 'z'))

export
namesGen' : Fuel -> Gen MaybeEmpty String
namesGen' _ = namesGen

genOneUniqueName : {l : Nat} -> Fuel -> (names: SVect l) -> (un: UniqNames l names) ->
                   Gen MaybeEmpty (out : String ** UniqNames (S l) (out :: names))
genOneUniqueName x names un = do
  (name ** uname) <- rawNewName x @{namesGen'} names un
  pure (name ** Cons names name un uname)

genNUniqueNames : {l : Nat} -> Fuel -> (n : Nat) -> (names: SVect l) -> (un: UniqNames l names) ->
                  Gen MaybeEmpty (newNames : SVect (n + l) ** UniqNames (n + l) newNames)
genNUniqueNames _ Z names un = pure (names ** un)
genNUniqueNames x (S k) names un = do
  (tail ** utail) <- genNUniqueNames x k names un
  (head ** uhead) <- genOneUniqueName x tail utail
  pure (head :: tail ** uhead)

export
genNUniqueNamesVect : Fuel -> (rl: Nat) -> {l: Nat} -> (names: SVect l) -> (un: UniqNames l names) ->
                      Gen MaybeEmpty (res: Vect rl String ** (newNames : SVect (rl + l) ** UniqNames (rl + l) newNames))
genNUniqueNamesVect x cnt names un = do
  (nNames ** nun) <- genNUniqueNames x cnt names un
  pure (take cnt $ toVect nNames ** nNames ** nun)
