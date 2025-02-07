module Test.Verilog.Pretty

import Data.Either
import Data.List
import Data.List.Extra
import Data.List1
import Data.List.Lazy

import Data.Fin.Split
import Data.Fuel
import Data.SortedMap
import public Data.Vect
import Data.Vect.Extra

import Data.Fin.ToFin

import public Test.Verilog

import Test.DepTyCheck.Gen
import System.Random.Pure.StdGen

import Text.PrettyPrint.Bernardy

import Syntax.IHateParens.List
import Syntax.IHateParens.SortedMap

%default total

totalInputs : {ms : ModuleSigsList} -> FinsList ms.length -> Nat
totalInputs = length . allInputs

totalOutputs : {ms : ModuleSigsList} -> FinsList ms.length -> Nat
totalOutputs = length . allOutputs

toTotalInputsIdx : {ms : _} -> {subMs : FinsList ms.length} ->
                   (idx : Fin subMs.asList.length) ->
                   Fin (index ms (index' subMs.asList idx)).inpsCount ->
                   Fin $ totalInputs {ms} subMs
toTotalInputsIdx {subMs=i::is} idx x with 0 (sym $ portsListAppendLen (index ms i).inputs (allInputs {ms} is))
                                        | 0 (length ((index ms i).inputs ++ allInputs {ms} is))
  toTotalInputsIdx FZ       x | Refl | _ = indexSum $ Left x
  toTotalInputsIdx (FS idx) x | Refl | _ = indexSum $ Right $ toTotalInputsIdx idx x

toTotalOutputsIdx : {ms : _} -> {subMs : FinsList ms.length} ->
                    (idx : Fin subMs.asList.length) ->
                    Fin (index ms $ index' subMs.asList idx).outsCount ->
                    Fin $ totalOutputs {ms} subMs
toTotalOutputsIdx {subMs=i::is} idx x with 0 (sym $ portsListAppendLen (index ms i).outputs (allOutputs {ms} is))
                                         | 0 (length ((index ms i).outputs ++ allOutputs {ms} is))
  toTotalOutputsIdx FZ       x | Refl | _ = indexSum $ Left x
  toTotalOutputsIdx (FS idx) x | Refl | _ = indexSum $ Right $ toTotalOutputsIdx idx x

connFwdRel : {ss, sk : PortsList} -> (cons: Connections ss sk) -> Vect (sk.length) $ Maybe $ Fin ss.length
connFwdRel []          = []
connFwdRel (sfs :: cs) = helper sfs :: connFwdRel cs where
  helper : SourceForSink ss sink -> Maybe $ Fin (length ss)
  helper NoSource                = Nothing
  helper (SingleSource srcIdx _) = Just srcIdx

||| Same as connFwdRel but repeated indexes are replaced to Nothing
connFwdUnique : Vect sk (Maybe (Fin ss)) -> (used: List (Fin ss)) -> Vect sk (Maybe (Fin ss))
connFwdUnique []        _    = []
connFwdUnique (x :: xs) used = case x of
  Just srcIdx => case find (== srcIdx) used of
    Nothing => Just srcIdx :: connFwdUnique xs (srcIdx::used)
    Just _  => Nothing     :: connFwdUnique xs used
  Nothing   => Nothing     :: connFwdUnique xs used

nothings : Vect sk (Maybe a) -> Nat
nothings []              = 0
nothings (Nothing :: xs) = S (nothings xs)
nothings (Just _  :: xs) = nothings xs

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

toVect : SVect l -> Vect l String
toVect [] = []
toVect (x :: xs) = x :: toVect xs

public export
fromVect : Vect l String -> SVect l
fromVect [] = []
fromVect (x :: xs) = x :: fromVect xs

public export
data UniqNames : (l : Nat) -> SVect l -> Type
public export
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

Show SVBasic where
  show Logic'   = "logic"
  show Wire'    = "wire"
  show Uwire'   = "uwire"
  show Int'     = "int"
  show Integer' = "integer"
  show Bit'     = "bit"
  show Real'    = "real"

||| Prints the type's signature to the left of the name
printVarOrPacked : SVType -> String
printVarOrPacked $ Var bt                = "\{show bt} "
printVarOrPacked $ Arr $ Packed t s e    = printVarOrPacked t ++ "[\{show s}:\{show e}]"
printVarOrPacked $ Arr $ Unpacked {t, _} = printVarOrPacked t

||| Prints the type's signature to the right of the name.
|||
||| 7.4.2
||| A fixed-size unpacked dimension may also be specified by a single positive constant integer expression to
||| specify the number of elements in the unpacked dimension, as in C. In this case, [size] shall mean the
||| same as [0:size-1].
||| ex:
||| int Array[0:7][0:31]; // array declaration using ranges
||| int Array[8][32];     // array declaration using sizes
|||
||| IEEE 1800-2023
printUnpackedDims : SVType -> String
printUnpackedDims $ Var _                = ""
printUnpackedDims $ Arr $ Packed {}      = ""
printUnpackedDims $ Arr $ Unpacked t s e = printUnpackedDims t ++ "[\{show s}:\{show e}]"

||| examples:
||| bit uP [3:0]; //1-D unpacked
||| bit [3:0] p;  //1-D packed
printSVArr : SVArray _ _ _ -> String -> String
printSVArr (Packed   svt s e) name = "\{printVarOrPacked svt}[\{show s}:\{show e}] \{name}"
printSVArr (Unpacked svt s e) name = "\{printVarOrPacked svt}\{space svt}\{name} \{printUnpackedDims svt}[\{show s}:\{show e}]" where
  space : SVType -> String
  space (Arr $ Packed {}) = " "
  space _           = ""

printConnType : SVType -> String -> String
printConnType (Arr arr) name = printSVArr arr name
printConnType (Var svt) name = "\{show svt} \{name}"

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

||| For standart gates in SystemVerilog only position-based connections are allowed.
||| For user modules, interfaces, primitives and programs both position-based and name-based connections are allowed.
||| This type stores the names of inputs and outputs, if they exist
public export
data InsOuts : (ins, outs : Nat) -> Type where
  StdModule  : (ins, outs : Nat) -> InsOuts ins outs
  UserModule : (inputs : Vect ins String) -> (outputs : Vect outs String) -> InsOuts ins outs

public export
record PrintableModule inps outs where
  constructor MkPrintableModule
  name    : String
  insOuts : InsOuts inps outs

namespace PrintableModules
  public export
  data PrintableModules : (ms : ModuleSigsList) -> Type where
    Nil  : PrintableModules []
    (::) : PrintableModule m.inpsCount m.outsCount -> PrintableModules ms -> PrintableModules (m :: ms)

  public export
  length : PrintableModules _ -> Nat
  length [] = Z
  length (l :: ls) = S $ length ls

  public export %inline
  (.length) : PrintableModules _ -> Nat
  (.length) = length

  public export
  index : {ms : _} -> (ps : PrintableModules ms) -> (fin: Fin ms.length) -> PrintableModule ((index ms fin).inpsCount) ((index ms fin).outsCount)
  index (m::_ ) FZ     = m
  index (_::ms) (FS i) = index ms i

nameBasedConnections : List String -> List String -> List String
nameBasedConnections = zipWith $ \external, internal => ".\{external}(\{internal})"

concatInpsOuts: {opts : _} -> List String -> List String -> Doc opts
concatInpsOuts inputs outputs = (tuple $ line <$> outputs ++ inputs) <+> symbol ';'

public export
allModuleNames : PrintableModules ms -> SVect ms.length
allModuleNames []        = []
allModuleNames (x :: xs) = x.name :: allModuleNames xs

genNUniqueNamesVect : Fuel -> (rl: Nat) -> {l: Nat} -> (names: SVect l) -> (un: UniqNames l names) ->
                      Gen MaybeEmpty (res: Vect rl String ** (newNames : SVect (rl + l) ** UniqNames (rl + l) newNames))
genNUniqueNamesVect x cnt names un = do
  (nNames ** nun) <- genNUniqueNames x cnt names un
  pure (take cnt $ toVect nNames ** nNames ** nun)

printConnections: String -> (cons: PortsList) -> Vect (cons.length) String -> List String
printConnections keyword cons names = zipWith (\conn, name => "\{keyword} \{printConnType conn name}") (toList cons) (toList names)

-- rewrite length from concatenated to separated PortsLists
sepLen : {a, b: PortsList} -> Vect (length (a ++ b)) c -> Vect (length a + length b) c
sepLen {a, b} v = rewrite portsListAppendLen a b in v
-- rewrite length from separated to concatenated PortsLists
comLen : {a, b: PortsList} -> Vect (length a + length b) c -> Vect (length (a ++ b)) c
comLen {a, b} v = rewrite sym $ portsListAppendLen a b in v

fillNames : Vect n (Maybe (Fin srcCount)) -> Vect srcCount String -> Vect x String -> Vect n String
fillNames []                _         _               = []
fillNames (Nothing  :: xs)  srcNames (fallback :: fs) = fallback :: fillNames xs srcNames fs
fillNames (Nothing  :: xs)  srcNames []               = "error"  :: fillNames xs srcNames []
fillNames (Just idx :: xs)  srcNames fs               = index idx srcNames :: fillNames xs srcNames fs

||| Name the sinks according to the source's index. Generate new names for missing indexes
resolveSinks: (idxs: Vect sk (Maybe $ Fin ss)) -> Vect ss String -> Fuel -> {l: Nat} -> (names: SVect l) -> (un: UniqNames l names) ->
              Gen MaybeEmpty (Vect sk String, (newNames : SVect (nothings idxs + l)  ** UniqNames (nothings idxs + l) newNames))
resolveSinks sinks srcNames x names un = do
  (fallbacks ** nNames ** nun) <- genNUniqueNamesVect x (nothings sinks) names un
  let res = fillNames sinks srcNames fallbacks
  pure (res, (nNames ** nun))

||| > In the absence of an explicit declaration, an implicit net of default net type shall be assumed
||| IEEE 1800-2023
|||
||| The default net type is wire. It could be changed to another net type using `default_nettype` directive.
||| Net types aren't compatible with unpacked arrays. So connections to unpacked array ports must be declared explicitly.
|||
||| Prints an explicit declaration for each submodule input that's not connected to any source
resolveUnpSI : Vect sk String -> List ((Fin sk, Maybe a), SVType) -> List String
resolveUnpSI names = mapMaybe resolve' where
  resolve' : ((Fin sk, Maybe a), SVType) -> Maybe String
  resolve' ((finSK, Nothing), Arr u@(Unpacked {})) = Just $ printSVArr u $ index finSK names
  resolve' _                                       = Nothing

||| Prints an explicit declaration for each submodule output connected to a submodule input or not connected at all.
||| Doesn't print declaration for ports connected to top outputs
resolveUnpSO : Foldable c => Foldable d => c String -> d (String, SVType) -> List String
resolveUnpSO tops = flip foldr [] $ \case
  (n, Arr u@(Unpacked {})) => if elem n tops then id else with Prelude.(::) (printSVArr u n ::)
  _ => id {a=List String}

||| filter `top inputs -> top outputs` connections
filterTITO : Vect n (Maybe (Fin ss)) -> (inps : Nat) -> Vect n (Maybe (Fin inps))
filterTITO []        _    = []
filterTITO (x :: xs) inps = tryToFit' x :: filterTITO xs inps where
  tryToFit' : Maybe (Fin from) -> Maybe $ Fin inps
  tryToFit' Nothing    = Nothing
  tryToFit' (Just fin) = tryToFit fin

printAssign : String -> String -> String
printAssign l r = "assign \{l} = \{r};"

||| It's impossible to connect top inputs to top outputs directly because top ports must have unique names.
||| However, such an assignment may be declared so that these ports can transmit values
|||
||| ex:
||| module m(output wire a, input wire b);
|||   assign a = b;
||| endmodule
resolveConAssigns : Vect sk (Maybe (Fin inps)) -> Vect sk String -> Vect inps String -> Vect sk (Maybe String)
resolveConAssigns v outNames inpNames = map (resolveConn outNames inpNames) $ withIndex v where
  resolveConn: Vect sk String -> Vect inps String -> (Fin sk, Maybe (Fin inps)) -> Maybe String
  resolveConn outNames inpNames (finOut, finInpM) = case finInpM of
    Nothing     => Nothing
    Just finInp => Just $ printAssign (index finOut outNames) (index finInp inpNames)

-- zip PortsList with List
zipPLWList : Foldable b => b a -> PortsList -> List (a, SVType)
zipPLWList other ports = toList other `zip` toList ports

export
prettyModules : {opts : _} -> {ms : _} -> Fuel ->
                (pms : PrintableModules ms) -> UniqNames ms.length (allModuleNames pms) => Modules ms -> Gen0 $ Doc opts
prettyModules x _         End = pure empty
prettyModules x pms @{un} (NewCompositeModule m subMs sssi cont) = do
  -- Generate submodule name
  (name ** isnew) <- rawNewName x @{namesGen'} (allModuleNames pms) un

  -- Generate top module input names
  (inputNames ** namesWithInputs ** uni) <- genNUniqueNamesVect x m.inpsCount (allModuleNames pms) un
  -- Generate submodule output names
  (subMONames ** namesWithSubOuts ** unis) <- genNUniqueNamesVect x (allOutputs {ms} subMs).length namesWithInputs uni
  -- Generate submodule instance names
  (subMInstanceNames ** namesWithSubMs ** uniosub) <- genNUniqueNamesVect x subMs.length namesWithSubOuts unis

  -- Resolve submodule inputs
  let allConns = connFwdRel sssi
  let (siss, tossRaw) = splitAt (allInputs {ms} subMs).length (sepLen allConns)
  (subMINames, (namesWithNoSources ** uniosubn)) <- resolveSinks siss (comLen $ inputNames ++ subMONames) x namesWithSubMs uniosub

  -- Resolve top outputs
  let toss = connFwdUnique tossRaw []
  (assignedInpNames ** namesWithTIN ** uniosubnt) <- genNUniqueNamesVect x m.inpsCount namesWithNoSources uniosubn
  (outputNames, (namesWithNoTopOuts ** uniosubnto)) <- resolveSinks toss (comLen $ assignedInpNames ++ subMONames) x namesWithTIN uniosubnt

  -- Resolve `top inputs -> top outputs` connections
  let (_ ** tito) = catMaybes $ resolveConAssigns (filterTITO toss m.inpsCount) outputNames inputNames

  -- Unpacked arrays declarations
  let unpackedDecls = resolveUnpSI subMINames (withIndex siss `zipPLWList` allInputs {ms} subMs)
                   ++ resolveUnpSO outputNames (subMONames `zipPLWList` allOutputs {ms} subMs)

  -- Save generated names
  let generatedPrintableInfo : ?
      generatedPrintableInfo = MkPrintableModule name (UserModule inputNames outputNames)

  -- Recursive call to use at the end
  recur <- prettyModules x (generatedPrintableInfo :: pms) cont
  pure $ vsep
    [ enclose (flush $ line "module" <++> line name) (line "endmodule:" <++> line name) $ flush $ indent 2 $ vsep $ do
      let outerModuleInputs = printConnections "input" m.inputs inputNames
      let outerModuleOutputs = printConnections "output" m.outputs outputNames
      let outerModuleIO = toList $ line <$> (outerModuleOutputs ++ outerModuleInputs)
      [ tuple outerModuleIO <+> symbol ';' , line "" ] ++
      (unpackedDecls <&> \(unp) : String => line unp <+> symbol ';') ++ [ line "" ] ++
        (zip (toList subMInstanceNames) (withIndex subMs.asList) <&> \(instanceName, subMsIdx, msIdx) =>
          line (index msIdx $ toVect (allModuleNames pms)) <++> line instanceName <+> do
            let inputs  = List.allFins (index ms $ index' subMs.asList subMsIdx).inpsCount <&> toTotalInputsIdx subMsIdx
            let outputs = List.allFins (index ms $ index' subMs.asList subMsIdx).outsCount <&> toTotalOutputsIdx subMsIdx

            let inputs  = inputs  <&> flip index subMINames
            let outputs = outputs <&> flip index subMONames

            let modulePrintable = index pms msIdx
            case modulePrintable.insOuts of
              StdModule  _        _         => concatInpsOuts inputs outputs
              UserModule exInputs exOutputs => do
                let inpsJoined = nameBasedConnections (toList exInputs)  inputs
                let outsJoined = nameBasedConnections (toList exOutputs) outputs

                concatInpsOuts inpsJoined outsJoined
        )
        ++ [ line "" ] ++ (map line $ toList tito)
    , line ""
    , recur
    ]
