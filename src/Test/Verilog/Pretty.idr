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

import public Test.Verilog.Module
import public Test.Verilog.Assign
import public Test.Verilog.Literal

import Test.DepTyCheck.Gen
import Text.PrettyPrint.Bernardy
import Syntax.IHateParens.List

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

public export
connFwdRel : {ss, sk : PortsList} -> (cons: Connections ss sk b) -> Vect (sk.length) $ Maybe $ Fin ss.length
connFwdRel Empty           = []
connFwdRel (Cons sfs cs _) = helper sfs :: connFwdRel cs where
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

||| rewrite length from separated to concatenated PortsLists
comLen : {a, b: PortsList} -> Vect (length a + length b) c -> Vect (length (a ++ b)) c
comLen {a, b} v = rewrite sym $ portsListAppendLen a b in v

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

public export
data ExtendedModules : ModuleSigsList -> Type where

  End : ExtendedModules ms
  ||| A module with assigns and literals
  NewCompositeModule :
    (m : ModuleSig) ->
    (subMs : FinsList ms.length) ->
    (sssi : Connections (m.inputs ++ allOutputs {ms} subMs) (allInputs {ms} subMs) False) ->
    (ssto : Connections (m.inputs ++ allOutputs {ms} subMs) (m.outputs)            True ) ->
    (assignsSInps : List $ Fin (allInputs {ms} subMs).length) ->
    (assignsTOuts : List $ Fin (m.outputs).length) ->
    (assignsSS : List $ Fin (m.inputs ++ allOutputs {ms} subMs).length) ->
    {pl : PortsList} ->
    (literals : LiteralsList pl) ->
    (cont : ExtendedModules $ m::ms) ->
    ExtendedModules ms

export
prettyModules : {opts : _} -> {ms : _} -> Fuel ->
                (pms : PrintableModules ms) -> UniqNames ms.length (allModuleNames pms) => ExtendedModules ms -> Gen0 $ Doc opts
prettyModules x _         End = pure empty
prettyModules x pms @{un} (NewCompositeModule m subMs sssi ssto assignsSInps assignsTOuts assignsSS literals cont) = do
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
        ++ [ line "", line "// Top inputs -> top outputs assigns" ] ++ (map line $ toList tito)
        ++ [ line "", line "// Assigns" ] ++ (map line assignments)
    , line ""
    , recur
    ]
