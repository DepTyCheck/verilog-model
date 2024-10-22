module Test.Verilog.Pretty

import Data.Either
import Data.List
import Data.List.Extra
import Data.List1
import Data.List.Lazy
import Data.String

import Data.Fin.Split
import Data.Fuel
import Data.SortedMap
import public Data.Vect
import Data.Vect.Extra

import public Test.Verilog

import Test.DepTyCheck.Gen
import System.Random.Pure.StdGen

import Text.PrettyPrint.Bernardy

import Syntax.IHateParens.List
import Syntax.IHateParens.SortedMap

%default total

toTotalInputsIdx : {ms : _} -> {subMs : FinsList ms.length} ->
                   (idx : Fin subMs.asList.length) ->
                   Fin (index ms (index' subMs.asList idx)).inputs ->
                   Fin $ totalInputs {ms} subMs
toTotalInputsIdx {subMs=i::_ } FZ       x = indexSum $ Left x
toTotalInputsIdx {subMs=i::is} (FS idx) x = indexSum $ Right $ toTotalInputsIdx idx x

toTotalOutputsIdx : {ms : _} -> {subMs : FinsList ms.length} ->
                    (idx : Fin subMs.asList.length) ->
                    Fin (index ms $ index' subMs.asList idx).outputs ->
                    Fin $ totalOutputs {ms} subMs
toTotalOutputsIdx {subMs=i::_ } FZ       x = indexSum $ Left x
toTotalOutputsIdx {subMs=i::is} (FS idx) x = indexSum $ Right $ toTotalOutputsIdx idx x

connName : Fin (m.inputs + totalOutputs {ms} subMs) -> String
connName n = "c\{show n}"

outputName : Fin (m.outputs + totalInputs {ms} subMs) -> String
outputName n = "o\{show n}"

isModuleInput : {m : _} -> Fin (m.inputs + totalOutputs {ms} subMs) -> Bool
isModuleInput = isLeft . splitSum

isModuleOutput : {m : _} -> Fin (m.outputs + totalInputs {ms} subMs) -> Bool
isModuleOutput = isLeft . splitSum

connFwdRel : Connections f t -> Vect t $ Fin f
connFwdRel []      = []
connFwdRel (i::cs) = i :: connFwdRel cs

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

toList : SVect l -> List String
toList [] = []
toList (x :: xs) = x :: toList xs

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
  "alias", "always", "always_comb", "always_ff", "always_latch", "and",
  "assert", "assign", "assume", "automatic", "before", "begin", "bind",
  "bins", "binsof", "bit", "break", "buf", "bufif0", "bufif1", "byte",
  "case", "casex", "casez", "cell", "chandle", "class", "clocking", "cmos",
  "config", "const", "constraint", "context", "continue", "cover", "covergroup",
  "coverpoint", "cross", "deassign", "default", "defparam", "design", "disable",
  "dist", "do", "edge", "else", "end", "endcase", "endclass", "endclocking",
  "endconfig", "endfunction", "endgenerate", "endgroup", "endinterface", "endmodule",
  "endpackage", "endprimitive", "endprogram", "endproperty", "endsequence", "endspecify",
  "endtable", "endtask", "enum", "event", "expect", "export", "extends", "extern", "final",
  "first_match", "for", "force", "foreach", "forever", "fork", "forkjoin", "function", "generate",
  "genvar", "highz0", "highz1", "if", "iff", "ifnone", "ignore_bins", "illegal_bins", "import",
  "incdir", "include", "initial", "inout", "input", "inside", "instance", "int", "integer",
  "interface", "intersect", "join", "join_any", "join_none", "large", "larger", "liblist",
  "library", "local", "localparam", "logic", "longint", "macromodule", "matches", "medium",
  "modport", "module", "nand", "negedge", "new", "nmos", "nor", "noshow-cancelled", "noshowcancelled",
  "not", "notif0", "notif1", "null", "or", "output", "package", "packed", "parameter", "pmos", "posedge",
  "primitive", "priority", "program", "property", "protected", "pull0", "pull1", "pulldown", "pullup",
  "pulsestyle_ondetect", "pulsestyle_onevent", "pure", "rand", "randc", "randcase", "randsequence", "rcmos",
  "real", "realtime", "ref", "reg", "release", "repeat", "return", "rnmos", "rpmos", "rtran", "rtranif0",
  "rtranif1", "scalared", "sequence", "shortint", "shortreal", "show-cancelled",
  "showcancelled", "signed", "small", "solve", "specify", "specpa", "specparam",
  "static", "string", "strong0", "strong1", "struct", "super", "supply0", "supply1",
  "table", "tagged", "task", "this", "throughout", "time", "timeprecision", "timeunit",
  "tran", "tranif0", "tranif1", "tri", "tri0", "tri1", "triand", "trior", "trireg", "type",
  "typedef", "union", "unique", "unsigned", "use", "uwire", "var", "vectored", "virtual",
  "void", "wait", "wait_order", "wand", "weak0", "weak1", "while", "wildcard", "wire",
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

invertConn : {ins: Nat} -> Vect outs (Fin ins) -> Vect ins (List (Fin outs))
invertConn = foldl registerDriven (replicate ins []) . withIndex
  where
    registerDriven : Vect ins (List (Fin outs)) -> (Fin outs, Fin ins) -> Vect ins (List (Fin outs))
    registerDriven ys (outIdx, inIdx) = updateAt inIdx (outIdx ::) ys

getFirstTopLevel : {topLevelOutputs: Nat} -> List (Fin (topLevelOutputs + subMInputs)) -> Maybe (Fin topLevelOutputs)
getFirstTopLevel [] = Nothing
getFirstTopLevel (x :: xs) = case isLT (finToNat x) topLevelOutputs of
  Yes a => Just $ natToFinLT $ finToNat x
  No b => getFirstTopLevel xs

-- Finds all inputs that drive no toplevel outputs
findInternalInputs :  {topLevelOutputs: Nat} ->
                      Vect l (Fin ins, List (Fin (topLevelOutputs + subMInputs))) ->
                      List (Fin ins)
findInternalInputs = map fst . filter (isNothing . getFirstTopLevel . snd) . toList

computeInternalsLookupTable : (internals : List (Fin l)) -> (names: Vect (internals.length) String) -> SortedMap (Fin l) String
computeInternalsLookupTable [] [] = empty
computeInternalsLookupTable (x :: xs) (y :: ys) = insert x y $ computeInternalsLookupTable xs ys

solveInputNames : {topLevelInputs: Nat} -> {topLevelOutputs: Nat} -> Vect topLevelInputs String -> Vect topLevelOutputs String ->
                  SortedMap (Fin (topLevelInputs + subMOutputs)) String ->
                  Vect (topLevelInputs + subMOutputs) (Fin (topLevelInputs + subMOutputs), List (Fin (topLevelOutputs + subMInputs))) ->
                  Vect (topLevelInputs + subMOutputs) String
solveInputNames iNames oNames lut = (iNames ++) . map solveName . drop topLevelInputs
  where
    solveName : (Fin (topLevelInputs + subMOutputs), List (Fin (topLevelOutputs + subMInputs))) -> String
    solveName (i, v) = fromMaybe "<error>" $ lookup i lut <|> flip index oNames <$> getFirstTopLevel v

solveOutputNames :  {topLevelOutputs: Nat} -> Vect ins String -> Vect topLevelOutputs String ->
                    Vect (topLevelOutputs + subMInputs) (Fin ins) ->
                    Vect (topLevelOutputs + subMInputs) String
solveOutputNames iNames oNames = (oNames ++) . map (flip index iNames) . drop topLevelOutputs

solveAssigns :  {topLevelOutputs: Nat} -> Vect ins String -> Vect topLevelOutputs String ->
                Vect (topLevelOutputs + subMInputs) (Fin ins) -> List (Fin topLevelOutputs, Fin ins)
solveAssigns iNames oNames = filter namesNotIdentical . toList . withIndex . take topLevelOutputs
  where
    namesNotIdentical : (Fin topLevelOutputs, Fin ins) -> Bool
    namesNotIdentical (outId, inId) = index outId oNames /= index inId iNames

public export
data ModuleType = STDVerilog  -- std gate
                | UserDefined -- module, interface, user-defined primitive, program

public export
record PrintableModuleInfo where
  constructor MkPrintableModuleInfo
  name     : String
  inputs   : Vect n String  -- Input names
  outputs  : Vect m String  -- Output names
  mtype    : ModuleType     -- is name based connection allowed?

data ConnectionStyle = PositionBased      -- ex. test0 test0Inst1 (h, i, j, a);
                     | ExplicitNameBased  -- ex. test0 test1Inst2 (.a(h), .b(i), .c(j), .d(a));
                    --  | ImplicitNameBased  -- ex. test0 test0Inst3 (.a, .b, .c);
                    --  | Wildcard           -- ex. test0 test0Inst4 (.*)

genConnectionStyle : Gen NonEmpty ConnectionStyle
genConnectionStyle = elements [PositionBased, ExplicitNameBased]

namesHelper: List String -> List String -> String
namesHelper external internal =
  do
  let s = zip external internal
  let s = map (\(external, internal) => "." ++ external ++ "(" ++ internal ++ ")") s
  joinBy ", " s

export
prettyModules : {opts : _} -> {ms : _} -> Fuel -> (names : SVect ms.length) -> UniqNames ms.length names => Vect (length ms) PrintableModuleInfo -> Modules ms -> Gen0 $ Doc opts
prettyModules x _ _ End  = pure empty
prettyModules x names @{un} pmis (NewCompositeModule m subMs conn cont)  = do
  -- Generate submodule name
  (name ** isnew) <- rawNewName x @{namesGen'} names un
  -- Generate toplevel input names
  (namesWithInput ** uni) <- genNUniqueNames x m.inputs names un
  let inputNames = take m.inputs $ toVect namesWithInput
  -- Generate toplevel output names
  (namesWithIO ** unio) <- genNUniqueNames x m.outputs namesWithInput uni
  let outputNames = take m.outputs $ toVect namesWithIO
  -- Generate submodule instance names
  (namesIOWithSubMs ** uniosub) <- genNUniqueNames x subMs.length namesWithIO unio
  let subMInstanceNames = take subMs.length $ toVect namesIOWithSubMs

  -- Extract a output to driving input mapping from conn
  let outputToDriver = connFwdRel conn
  -- Invert it into a input to driven outputs mapping
  let inputToDriven : Vect (m.inputs + totalOutputs subMs) $ List $ Fin $ m.outputs + totalInputs subMs
                    = invertConn outputToDriver

  -- Generate names for internal inputs (inputs that drive no toplevel outputs)
  let internalInputs = findInternalInputs $ withIndex inputToDriven
  (namesWithInternal ** unint) <- genNUniqueNames x internalInputs.length namesIOWithSubMs uniosub
  let internalInputNames = take internalInputs.length $ toVect namesWithInternal

  -- Create a full list of input names
  let internalLUT = computeInternalsLookupTable internalInputs internalInputNames
  let fullInputNames : Vect (m.inputs + totalOutputs subMs) String
                     = solveInputNames inputNames outputNames internalLUT $ withIndex inputToDriven
  let subMONames = drop m.inputs fullInputNames
  -- Create a full list of output names
  let fullOutputNames : Vect (m.outputs + totalInputs subMs) String
                      = solveOutputNames fullInputNames outputNames outputToDriver
  let subMINames = drop m.outputs fullOutputNames
  -- Compute necessary assign statements
  let assigns = solveAssigns fullInputNames outputNames outputToDriver

  let thisModuleInfo = MkPrintableModuleInfo name inputNames outputNames UserDefined
  -- Recursive call to use at the end
  recur <- prettyModules x (name::names) (thisModuleInfo::pmis) cont
  pure $ vsep
    [ enclose (flush $ line "module" <++> line name) (line "endmodule:" <++> line name) $ flush $ indent 2 $ vsep $ do
      let outerModuleInputs = map ("input logic " ++) inputNames
      let outerModuleOutputs = map ("output logic " ++) outputNames
      let outerModuleIO = toList $ line <$> (outerModuleOutputs ++ outerModuleInputs)
      [ tuple outerModuleIO <+> symbol ';' , line "" ] ++
        (zip (toList subMInstanceNames) (withIndex subMs.asList) <&> \(instanceName, subMsIdx, msIdx) => line (index msIdx $ toVect names) <++> line instanceName <+>
          do
          let moduleSig = index ms msIdx
          let modulePrintable = index msIdx pmis

          let inputs  = List.allFins (index ms $ index' subMs.asList subMsIdx).inputs  <&> toTotalInputsIdx subMsIdx
          let outputs = List.allFins (index ms $ index' subMs.asList subMsIdx).outputs <&> toTotalOutputsIdx subMsIdx

          let inputs  = inputs  <&> flip index subMINames
          let outputs = outputs <&> flip index subMONames

          case modulePrintable.mtype of
            STDVerilog  => (tuple $ line <$> outputs ++ inputs) <+> symbol ';'
            UserDefined =>
              do
              let externalInputs = modulePrintable.inputs
              let externalOutputs = modulePrintable.outputs

              let inpsJoined = namesHelper (toList externalInputs)  inputs
              let outsJoined = namesHelper (toList externalOutputs) outputs

              let res = if inpsJoined == "" || outsJoined == "" then inpsJoined ++ outsJoined
                                                                else inpsJoined ++ ", " ++ outsJoined

              line ("(" ++ res ++ ")") <+> symbol ';'
        ) ++
        [line ""] ++ (assigns <&> \(outIdx, inIdx) =>
          line "assign" <++> line (index outIdx outputNames) <++> symbol '=' <++> line (index inIdx fullInputNames) <+> symbol ';'
        )
    , line ""
    , recur
    ]
