module Test.Verilog.Pretty

import Data.Either
import Data.List
import Data.List.Extra
import Data.List1
import Data.List.Lazy
import Data.String

import Data.Fin.Split
import Data.Fuel
import public Data.Vect
import Data.Vect.Extra

import Data.Fin.ToFin

import public Test.Verilog.UniqueNames.Derived

import public Test.Verilog.SVType
import public Test.Verilog.Connections
import public Test.Verilog.CtxPorts
import public Test.Verilog.Assign
import public Test.Verilog.Literal

import Test.DepTyCheck.Gen
import Text.PrettyPrint.Bernardy
import Syntax.IHateParens.List

%default total

public export
toTotalInputsIdx : {ms : _} -> {subMs : FinsList ms.length} ->
                  (idx : Fin subMs.asList.length) ->
                  Fin (index ms (index' subMs.asList idx)).inpsCount ->
                  Fin $ totalInputs {ms} subMs
toTotalInputsIdx {subMs=i::is} idx x with 0 (sym $ portsListAppendLen (index ms i).inputs (allInputs {ms} is))
                                        | 0 (length ((index ms i).inputs ++ allInputs {ms} is))
  toTotalInputsIdx FZ       x | Refl | _ = indexSum $ Left x
  toTotalInputsIdx (FS idx) x | Refl | _ = indexSum $ Right $ toTotalInputsIdx idx x

public export
toTotalOutputsIdx : {ms : _} -> {subMs : FinsList ms.length} ->
                    (idx : Fin subMs.asList.length) ->
                    Fin (index ms $ index' subMs.asList idx).outsCount ->
                    Fin $ totalOutputs {ms} subMs
toTotalOutputsIdx {subMs=i::is} idx x with 0 (sym $ portsListAppendLen (index ms i).outputs (allOutputs {ms} is))
                                         | 0 (length ((index ms i).outputs ++ allOutputs {ms} is))
  toTotalOutputsIdx FZ       x | Refl | _ = indexSum $ Left x
  toTotalOutputsIdx (FS idx) x | Refl | _ = indexSum $ Right $ toTotalOutputsIdx idx x
  
public export
connFwdRel : {ss, sk : PortsList} -> (cons: Connections ss sk b tIs) -> Vect (sk.length) $ Maybe $ Fin ss.length
connFwdRel Empty           = []
connFwdRel (Cons sfs cs) = helper sfs :: connFwdRel cs where
  helper : SourceForSink ss sink -> Maybe $ Fin (length ss)
  helper NoSource             = Nothing
  helper (HasSource srcIdx _) = Just srcIdx

nothings : Vect sk (Maybe a) -> Nat
nothings []              = 0
nothings (Nothing :: xs) = S (nothings xs)
nothings (Just _  :: xs) = nothings xs


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
printUnpackedDims $ Arr $ Unpacked t s e = "[\{show s}:\{show e}]" ++ printUnpackedDims t

||| examples:
||| bit uP [3:0]; //1-D unpacked
||| bit [3:0] p;  //1-D packed
printSVArr : SVArray _ _ _ -> String -> String
printSVArr (Packed   svt s e) name = "\{printVarOrPacked svt}[\{show s}:\{show e}] \{name}"
printSVArr (Unpacked svt s e) name = "\{printVarOrPacked svt}\{space svt}\{name} [\{show s}:\{show e}]\{printUnpackedDims svt}" where
  space : SVType -> String
  space (Arr $ Packed {}) = " "
  space _           = ""

printConnType : SVType -> String -> String
printConnType (Arr arr) name = printSVArr arr name
printConnType (Var svt) name = "\{show svt} \{name}"

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

printConnections: String -> (cons: PortsList) -> Vect (cons.length) String -> List String
printConnections keyword cons names = zipWith (\conn, name => "\{keyword} \{printConnType conn name}") (toList cons) (toList names)

fillNames : Vect n (Maybe $ Fin srcCount) -> Vect srcCount String -> Vect x String -> Vect n String
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

printAssigns : List (String, String) -> List String
printAssigns []             = []
printAssigns ((l, r) :: xs) = printAssign l r :: printAssigns xs

||| It's impossible to connect top inputs to top outputs directly because top ports must have unique names.
||| However, such an assignment may be declared so that these ports can transmit values
|||
||| ex:
||| module a(output int o1, output int o2, input int i1);
|||   assign o1 = i1;
|||   assign o2 = i1;
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

||| Line of bits
printLinear : BinaryList t l -> String
printLinear [] = ""
printLinear (x :: xs) = printLinear' x ++ printLinear xs where
  printLinear' : Binary t' -> String
  printLinear' (Single y) = show y
  printLinear' (PArr y) = printLinear y
  printLinear' (UArr y) = printLinear y

toListStr : BinaryList t l -> (Binary t -> String) -> List String
toListStr []        _ = []
toListStr (x :: xs) f = f x :: toListStr xs f

||| Single x example:
||| logic m;
||| assign m = 'b1;
||| TODO: print literals of different random lengths
||| TODO: print the length of literal sometimes
|||
||| UArr x example:
||| logic m [1:0][4:0];
||| assign m = '{'{'b1,'b0,'b1,'b0,'b1},'{'b0,'b1,'b0,'b1,'b0}};
|||
||| PArr x example:
||| logic [1:0][4:0] m;
||| assign m = 'b01010101;
printBinary: Binary t -> String
printBinary (Single x) = "'b\{show x}"
printBinary (UArr   x) = "'{\{joinBy "," $ toListStr x printBinary}}"
printBinary (PArr   x) = "'b\{printLinear x}"

printLiterals : LiteralsList ls -> List String
printLiterals []        = []
printLiterals (b :: xs) = printBinary b :: printLiterals xs

getNames : Vect l String -> (List $ Fin l) -> List String
getNames names []        = []
getNames names (x :: xs) = index x names :: getNames names xs

typeOf' : List SVType -> Fin a -> Maybe SVType
typeOf' []      _      = Nothing
typeOf' (x::xs) FZ     = Just x
typeOf' (x::xs) (FS i) = typeOf' xs i

allInputs' : ModuleSigsList -> List SVType
allInputs' []      = []
allInputs' (m::ms) = (toList m.inputs) ++ allInputs' ms

allOutputs' : ModuleSigsList -> List SVType
allOutputs' []      = []
allOutputs' (m::ms) = (toList m.outputs) ++ allOutputs' ms

namespace Wanings

  export
  bitsCnt' : SVBasic -> Nat
  bitsCnt' Logic'   = 1
  bitsCnt' Wire'    = 1
  bitsCnt' Uwire'   = 1
  bitsCnt' Int'     = 32
  bitsCnt' Integer' = 32
  bitsCnt' Bit'     = 1
  bitsCnt' Real'    = 64

  export
  bitsCnt : SVType -> Nat
  bitsCnt (Var b)                = bitsCnt' b
  bitsCnt (Arr $ Unpacked t _ _) = bitsCnt t
  bitsCnt (Arr $ Packed t s e)   = S (max s e `minus` min s e) * bitsCnt t

  export
  printTruncationWarning : SVType -> String -> SVType -> String -> Maybe String
  printTruncationWarning op on np nn = do
    let oldb = bitsCnt op
    let newb = bitsCnt np
    case oldb == newb of
      True  => Nothing
      False => Just "// warning: implicit conversion of port connection \{specialWord oldb newb} from \{show oldb} to \{show newb} bits" where
        specialWord : (oldb : Nat) -> (newb : Nat) -> String
        specialWord o n =  if o > n then "truncates" else "expands"

  export
  isSigned : SVBasic -> Bool
  isSigned Logic'   = False
  isSigned Wire'    = False
  isSigned Uwire'   = False
  isSigned Int'     = True
  isSigned Integer' = True
  isSigned Bit'     = False
  isSigned Real'    = True

  export
  superBasic : SVType -> SVBasic
  superBasic (Arr (Unpacked t s e)) = superBasic t
  superBasic (Arr (Packed t s e)) = superBasic t
  superBasic (Var x) = x

  export
  printSignednessWarning : SVType -> String -> SVType -> String -> Maybe String
  printSignednessWarning op on np nn = do
    let olds = isSigned $ superBasic op
    let news = isSigned $ superBasic np
    case olds == news of
      True  => Nothing
      False => Just "// warning: implicit conversion changes signedness from \{specialWorld olds} to \{specialWorld news}" where
        specialWorld : Bool -> String
        specialWorld isSigned = if isSigned then "signed" else "unsigned"

  export
  states : SVBasic -> Nat
  states Logic'   = 4
  states Wire'    = 4
  states Uwire'   = 4
  states Int'     = 2
  states Integer' = 4
  states Bit'     = 2
  states Real'    = 2

  export
  printStatesWarning : SVType -> String -> SVType -> String -> Maybe String
  printStatesWarning op on np nn = do
    let olds = states $ superBasic op
    let news = states $ superBasic np
    case olds == news of
      True  => Nothing
      False => Just "// warning: implicit conversion changes possible bit states from \{show olds}-state to \{show news}-state"

parameters {opts : LayoutOpts} (m : ModuleSig) (ms: ModuleSigsList)  (subMs : FinsList ms.length) (pms : PrintableModules ms) 
           (subMINames : Vect (length $ allInputs {ms} subMs) String) (subMONames : Vect (length $ allOutputs {ms} subMs) String)
           (ports : ModuleSigsList)

  printSubmodules : List String -> List (Fin (length (subMs.asList)), Fin (length ms)) -> List $ Doc opts
  printSubmodules  subMInstanceNames subMsIdxs = foldl (++) [] $ map printSubm $ zip subMInstanceNames subMsIdxs where
    printImplicitCast : SVType -> String -> SVType -> String -> List String
    printImplicitCast op on np nn = do
      let warnings = catMaybes [ printTruncationWarning op on np nn, printSignednessWarning op on np nn, printStatesWarning op on np nn ]
      case isNil warnings of
        True  => []
        False => warnings ++ [ "//   \{printConnType op on} -> \{printConnType np nn}" ]

    printAllImplicitCasts : List SVType -> List String -> List SVType -> List String -> List String
    printAllImplicitCasts (p::ps) (n::ns) (p'::ps') (n'::ns') = do
      let curr = printImplicitCast p n p' n'
      let rest = printAllImplicitCasts ps ns ps' ns'
      case isNil curr of
        True  => rest
        False => if isNil rest then curr else curr ++ [ "//" ] ++ rest
    printAllImplicitCasts _       _       _         _         = []

    printSubm' : (pre : Doc opts) -> (siNames : List String) -> (soNames : List String) -> (exM : ModuleSig) ->
                 (ctxInps : List SVType) -> (ctxOuts : List SVType) -> (exInps : List String) -> (exOuts : List String) -> List (Doc opts)
    printSubm' pre siNames soNames exM ctxInps ctxOuts exInps exOuts = do
      let warningsSubOuts = printAllImplicitCasts (toList exM.outputs) exOuts ctxOuts soNames
      let warningsSubInps = printAllImplicitCasts ctxInps siNames (toList exM.inputs) exInps
      let warnings = if isNil warningsSubOuts || 
                        isNil warningsSubInps then warningsSubOuts ++ warningsSubInps else warningsSubOuts ++ [ "//" ] ++ warningsSubInps
      case isNil warnings of
        True  => [ pre, line "" ]
        False => [ pre ] ++ map line warnings ++ [ line "" ]

    printSubm : (String, (Fin (length (subMs.asList)), Fin (length ms))) -> List $ Doc opts
    printSubm (instanceName, subMsIdx, msIdx) = do
      let pre : Doc opts = line (index msIdx $ toVect $ allModuleNames pms) <++> line instanceName

      let inputs  = List.allFins (index ms $ index' subMs.asList subMsIdx).inpsCount <&> toTotalInputsIdx subMsIdx
      let outputs = List.allFins (index ms $ index' subMs.asList subMsIdx).outsCount <&> toTotalOutputsIdx subMsIdx

      let siNames = inputs  <&> flip index subMINames
      let soNames = outputs <&> flip index subMONames

      let ctxInps = catMaybes $ map (\x => typeOf' (allInputs'  ports) x) inputs
      let ctxOuts = catMaybes $ map (\x => typeOf' (allOutputs' ports) x) outputs

      let modulePrintable = index pms msIdx
      case modulePrintable.insOuts of
        StdModule  _      _      => printSubm' (pre <+> concatInpsOuts siNames soNames) siNames soNames (index ms msIdx) ctxInps ctxOuts siNames soNames
        UserModule exInps exOuts => do
          let inpsJoined = nameBasedConnections (toList exInps) siNames
          let outsJoined = nameBasedConnections (toList exOuts) soNames

          printSubm' (pre <+> concatInpsOuts inpsJoined outsJoined) siNames soNames (index ms msIdx) ctxInps ctxOuts (toList exInps) (toList exOuts)

public export
data ExtendedModules : ModuleSigsList -> Type where

  End : ExtendedModules ms
  ||| A module with assigns and literals
  NewCompositeModule :
    (m : ModuleSig) ->
    (subMs : FinsList ms.length) ->
    (sssi : Connections (m.inputs ++ allOutputs {ms} subMs) (allInputs {ms} subMs) False m.inpsCount) ->
    (ssto : Connections (m.inputs ++ allOutputs {ms} subMs) (m.outputs)            True  m.inpsCount) ->
    (assignsSInps : List $ Fin (allInputs {ms} subMs).length) ->
    (assignsTOuts : List $ Fin (m.outputs).length) ->
    (assignsSS : List $ Fin (m.inputs ++ allOutputs {ms} subMs).length) ->
    {pl : PortsList} ->
    (literals : LiteralsList pl) ->
    (cont : ExtendedModules $ m::ms) ->
    (ports : ModuleSigsList) ->
    ExtendedModules ms

export
prettyModules : {opts : _} -> {ms : _} -> Fuel ->
                (pms : PrintableModules ms) -> UniqNames ms.length (allModuleNames pms) => ExtendedModules ms -> Gen0 $ Doc opts
prettyModules x _         End = pure empty
prettyModules x pms @{un} (NewCompositeModule m subMs sssi ssto assignsSInps assignsTOuts assignsSS literals cont ports) = do
  -- Generate submodule name
  (name ** isnew) <- rawNewName x @{namesGen'} (allModuleNames pms) un

  -- Generate top module input names
  (inputNames ** namesWithInputs ** uni) <- genNUniqueNamesVect x m.inpsCount (allModuleNames pms) un
  -- Generate submodule output names
  (subMONames ** namesWithSubOuts ** unis) <- genNUniqueNamesVect x (allOutputs {ms} subMs).length namesWithInputs uni
  -- Generate submodule instance names
  (subMInstanceNames ** namesWithSubMs ** uniosub) <- genNUniqueNamesVect x subMs.length namesWithSubOuts unis

  -- Resolve submodule inputs
  let siss = connFwdRel sssi
  (subMINames, (namesWithNoSources ** uniosubn)) <- resolveSinks siss (comLen $ inputNames ++ subMONames) x namesWithSubMs uniosub

  -- Resolve top outputs
  let toss = connFwdRel ssto
  (assignedInpNames ** namesWithTIN ** uniosubnt) <- genNUniqueNamesVect x m.inpsCount namesWithNoSources uniosubn
  (outputNames, (namesWithNoTopOuts ** uniosubnto)) <- resolveSinks toss (comLen $ assignedInpNames ++ subMONames) x namesWithTIN uniosubnt

  -- Resolve `top inputs -> top outputs` connections
  let (_ ** tito) = catMaybes $ resolveConAssigns (filterTITO toss m.inpsCount) outputNames inputNames

  -- Unpacked arrays declarations
  let unpackedDecls = resolveUnpSI subMINames (withIndex siss `zipPLWList` allInputs {ms} subMs)
                   ++ resolveUnpSO outputNames (subMONames `zipPLWList` allOutputs {ms} subMs)

  -- Resolve assigns
  let assignments = printAssigns $ zip (getNames subMINames  assignsSInps
                                     ++ getNames outputNames assignsTOuts
                                     ++ getNames (comLen $ inputNames ++ subMONames) assignsSS) $ printLiterals literals

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
      printSubmodules m ms subMs pms subMINames subMONames ports (toList subMInstanceNames) (withIndex subMs.asList) ++
      [ line "", line "// Top inputs -> top outputs assigns" ] ++ (map line $ toList tito) ++
      [ line "", line "// Assigns" ] ++ (map line assignments)
    , line ""
    , recur
    ]
