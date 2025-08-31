module Test.Verilog.Pretty

import Data.Either
import Data.List
import Data.List.Extra
import Data.List1
import Data.List.Lazy
import Data.String
import Data.List.Elem

import Data.Fin.Split
import Data.Fuel
import public Data.Vect
import Data.Vect.Extra

import Data.Fin.ToFin

import public Test.Verilog.UniqueNames.Derived

import public Test.Verilog.SVType
import public Test.Verilog.Connections
import public Test.Verilog.Assign
import public Test.Verilog.Literal
import public Test.Verilog.TMPExpression
import public Test.Verilog.Warnings

import Test.DepTyCheck.Gen
import Text.PrettyPrint.Bernardy
import Syntax.IHateParens.List

%default total

public export
toTotalInputsIdx : {ms : _} -> {subMs : FinsList ms.length} ->
                  (idx : Fin subMs.asList.length) ->
                  Fin (index ms (index' subMs.asList idx)).inpsCount ->
                  Fin $ totalInputs {ms} subMs
toTotalInputsIdx {subMs=i::is} idx x with 0 (sym $ svolistAppendLen (index ms i).inputs (allInputs {ms} is))
                                        | 0 (length ((index ms i).inputs ++ allInputs {ms} is))
  toTotalInputsIdx FZ       x | Refl | _ = indexSum $ Left x
  toTotalInputsIdx (FS idx) x | Refl | _ = indexSum $ Right $ toTotalInputsIdx idx x

public export
toTotalOutputsIdx : {ms : _} -> {subMs : FinsList ms.length} ->
                    (idx : Fin subMs.asList.length) ->
                    Fin (index ms $ index' subMs.asList idx).outsCount ->
                    Fin $ totalOutputs {ms} subMs
toTotalOutputsIdx {subMs=i::is} idx x with 0 (sym $ svolistAppendLen (index ms i).outputs (allOutputs {ms} is))
                                         | 0 (length ((index ms i).outputs ++ allOutputs {ms} is))
  toTotalOutputsIdx FZ       x | Refl | _ = indexSum $ Left x
  toTotalOutputsIdx (FS idx) x | Refl | _ = indexSum $ Right $ toTotalOutputsIdx idx x

Show NetType where
  show Supply0' = "supply0"
  show Supply1' = "supply1"
  show Triand'  = "triand"
  show Trior'   = "trior"
  show Trireg'  = "trireg"
  show Tri0'    = "tri0"
  show Tri1'    = "tri1"
  show Uwire'   = "uwire"
  show Wire'    = "wire"
  show Tri'     = "tri"
  show Wand'    = "wand"
  show Wor'     = "wor"

Show NonIntegerType where
  show Shortreal' = "shortreal"
  show Real'      = "real"
  show Realtime'  = "realtime"

Show IntegerVectorType where
  show Bit'   = "bit"
  show Logic' = "logic"
  show Reg'   = "reg"

Show IntegerAtomType where
  show Byte'     = "byte"
  show Shortint' = "shortint"
  show Int'      = "int"
  show Longint'  = "longint"
  show Integer'  = "integer"
  show Time'     = "time"

Show S2Value where
  show Z = "0"
  show S = "1"

Show S4Value where
  show Z = "0"
  show S = "1"
  show X = "x"
  show H = "z"

Show (Binary s) where
  show (B2 b) = show b
  show (B4 b) = show b

Show (BinaryVect l s) where
  show bv = "'b\{showLinear bv}" where
    showLinear : BinaryVect l' s -> String
    showLinear []        = ""
    showLinear (x :: xs) = show x ++ showLinear xs 

Show (TypeLiteralVect l t)

||| Single bit example:
||| logic m;
||| assign m = 'b1;
||| TODO: print literals of different random lengths
||| TODO: print the length of literal sometimes
|||
||| UAL x example:
||| logic m [1:0][4:0];
||| assign m = '{'{'b1,'b0,'b1,'b0,'b1},'{'b0,'b1,'b0,'b1,'b0}};
|||
||| PAL x example:
||| logic [1:0][4:0] m;
||| assign m = 'b01010101;
Show (TypeLiteral sv) where
  show (RL  x) = show x
  show (SL  x) = show x
  show (VL  x) = show x
  show (PAL x) = show x
  show (UAL x) = show x

Show (TypeLiteralVect l t) where
  show x = "'{\{joinBy "," $ map show $ toList x}}"

||| print name
pn : String -> String
pn "" = ""
pn a = " \{a}"

showPackedSVT : SVType -> String
showPackedSVT (RVar x)              = show x
showPackedSVT (SVar x)              = show x
showPackedSVT (VVar x)              = show x
showPackedSVT (PackedArr t {p} s e) = "\{showPackedSVT t}\{space}[\{show s}:\{show e}]" where
  space : String
  space = case p of
    PA => ""
    PS => " "
showPackedSVT (UnpackedArr t   s e) = ""

||| examples:
||| bit uP [3:0]; //1-D unpacked
||| bit [3:0] p;  //1-D packed
|||
||| 7.4.2
||| A fixed-size unpacked dimension may also be specified by a single positive constant integer expression to
||| specify the number of elements in the unpacked dimension, as in C. In this case, [size] shall mean the
||| same as [0:size-1].
||| ex:
||| int Array[0:7][0:31]; // array declaration using ranges
||| int Array[8][32];     // array declaration using sizes
showSVType : SVType -> (name : String) -> String
showSVType rv@(RVar x)                name = "\{showPackedSVT rv}\{pn name}"
showSVType sv@(SVar x)                name = "\{showPackedSVT sv}\{pn name}"
showSVType vv@(VVar x)                name = "\{showPackedSVT vv}\{pn name}"
showSVType pa@(PackedArr   t {p} s e) name = "\{showPackedSVT t}\{space}[\{show s}:\{show e}]\{pn name}" where
  space : String
  space = case p of
    PA => ""
    PS => " "
showSVType ua@(UnpackedArr t     s e) name = "\{showPackedSVT $ basic t} \{name} [\{show s}:\{show e}]\{unpDimensions t}" where
  basic : SVType -> SVType
  basic (UnpackedArr t _ _) = basic t
  basic t                   = t
  
  unpDimensions : SVType -> String
  unpDimensions (UnpackedArr t s e) = "[\{show s}:\{show e}]" ++ unpDimensions t
  unpDimensions _                   = ""

showSVObj : SVObject -> (name : String) -> String
showSVObj (Net nt t) name = "\{show nt} \{showSVType t name}"
showSVObj (Var    t) name = showSVType t name

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

concatInpsOuts : {opts : _} -> List String -> List String -> Doc opts
concatInpsOuts inputs outputs = (tuple $ line <$> outputs ++ inputs) <+> symbol ';'

public export
allModuleNames : PrintableModules ms -> SVect ms.length
allModuleNames []        = []
allModuleNames (x :: xs) = x.name :: allModuleNames xs

printConnections : String -> (cons: SVObjList) -> Vect (cons.length) String -> List String
printConnections keyword cons names = zipWith (\conn, name => "\{keyword} \{showSVObj conn name}") (toList cons) (toList names)

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

printTMPExpr : Vect (length mcs) String -> TMPExpression mcs t -> String
printTMPExpr _     (MkLiteral  l) = show l
printTMPExpr names (MkQualName f) = index f names

printTMPExprs : Vect (length mcs) String -> TMPExList mcs fs -> List String
printTMPExprs _     []        = []
printTMPExprs names (b :: xs) = printTMPExpr names b :: printTMPExprs names xs

getNames : Vect l String -> FinsList l -> List String
getNames names []        = []
getNames names (x :: xs) = index x names :: getNames names xs

isElem : Eq a => (x : a) -> (xs : List a) -> Bool
isElem x []      = False
isElem x (y::xs) = case x == y of
  True => True
  False => isElem x xs

iMcsByF : (mcs : MultiConnectionsList ms m subMs) ->
          (extractField : MultiConnection ms m subMs -> List $ Fin iport) -> Fin iport -> Maybe (Fin $ length mcs)
iMcsByF mcs func fin = findIndex resolve $ toVect mcs where
    resolve : MultiConnection ms m subMs -> Bool
    resolve sc = isElem fin $ func sc

findMcsNameByF : (extractField : MultiConnection ms m subMs -> List $ Fin iport) -> 
                 (mcs : MultiConnectionsList ms m subMs) -> Vect (length mcs) String -> Fin iport -> String
findMcsNameByF extract mcs mcsNames fin = case iMcsByF mcs extract fin of
  Just mcsFin => index mcsFin mcsNames
  Nothing     => "error!!"

extractSubSnks : MultiConnection ms m subMs -> List (Fin $ subSnks' ms m subMs)
extractSubSnks (MkMC _ ssk _ _) = ssk.asList

extractSubSrcs : MultiConnection ms m subMs -> List (Fin $ subSrcs' ms m subMs)
extractSubSrcs (MkMC _ _ _ ssc) = ssc.asList

extractTopSrcs : MultiConnection ms m subMs -> List (Fin m.inpsCount)
extractTopSrcs (MkMC _ _ (Just i) _) = [ i ]
extractTopSrcs _                     = []

extractTopSnks : MultiConnection ms m subMs -> List (Fin m.outsCount)
extractTopSnks (MkMC (Just i) _ _ _) = [ i ]
extractTopSnks _                     = []

findSIName : (mcs : MultiConnectionsList ms m subMs) -> (mcsNames : Vect (length mcs) String) -> Fin (subSnks' ms m subMs) -> String
findSIName = findMcsNameByF extractSubSnks

findSOName : (mcs : MultiConnectionsList ms m subMs) -> (mcsNames : Vect (length mcs) String) -> Fin (subSrcs' ms m subMs) -> String
findSOName = findMcsNameByF extractSubSrcs

findTIName : (mcs : MultiConnectionsList ms m subMs) -> Vect (length mcs) String -> Fin (m.inpsCount) -> String
findTIName = findMcsNameByF extractTopSrcs

findTOName : (mcs : MultiConnectionsList ms m subMs) -> Vect (length mcs) String -> Fin (m.outsCount) -> String
findTOName = findMcsNameByF extractTopSnks

findSubPortType : (MultiConnection ms m subMs -> List $ Fin $ subs) ->
                  MultiConnectionsList ms m subMs -> Fin subs -> SVObject
findSubPortType extract mcs fin = case iMcsByF mcs extract fin of
  Just mcsFin => typeOf $ index mcs mcsFin
  Nothing     => defaultNetType -- error. should not be possible

findSISVT : MultiConnectionsList ms m subMs -> Fin (subSnks' ms m subMs) -> SVObject
findSISVT = findSubPortType extractSubSnks

findSOSVT : MultiConnectionsList ms m subMs -> Fin (subSrcs' ms m subMs) -> SVObject
findSOSVT = findSubPortType extractSubSrcs


parameters {opts : LayoutOpts} (m : ModuleSig) (ms: ModuleSigsList)  (subMs : FinsList ms.length) (pms : PrintableModules ms)
           (mcs : MultiConnectionsList ms m subMs) (mcsNames : Vect (length mcs) String)

  printSubmodules : List String -> List (Fin (length (subMs.asList)), Fin (length ms)) -> List $ Doc opts
  printSubmodules  subMInstanceNames subMsIdxs = foldl (++) [] $ map printSubm $ zip subMInstanceNames subMsIdxs where

    printSubm' : (pre : Doc opts) -> (siNames : List String) -> (soNames : List String) -> (exM : ModuleSig) ->
                 (ctxInps : List SVObject) -> (ctxOuts : List SVObject) -> (exInps : List String) -> (exOuts : List String) -> List (Doc opts)
    printSubm' pre siNames soNames exM ctxInps ctxOuts exInps exOuts = do
      let warningsSubOuts = printAllImplicitCasts showSVObj (toList exM.outputs) exOuts ctxOuts soNames
      let warningsSubInps = printAllImplicitCasts showSVObj ctxInps siNames (toList exM.inputs) exInps  
      let warnings = if isNil warningsSubOuts || 
                        isNil warningsSubInps then warningsSubOuts ++ warningsSubInps else warningsSubOuts ++ [ "//" ] ++ warningsSubInps
      case isNil warnings of
        True  => [ pre, line "" ]
        False => [ pre ] ++ map line warnings ++ [ line "" ]

    ||| 23.2.1 Module header definition
    ||| The module header defines the following:
    ||| — The name of the module
    ||| — The port list of the module
    ||| — The direction and size of each port
    ||| — The type of data passed through each port
    ||| — The parameter constants of the module
    ||| — A package import list of the module
    ||| — The default lifetime (static or automatic) of subroutines defined within the module
    ||| IEEE 1800-2023
    printSubm : (String, (Fin (length (subMs.asList)), Fin (length ms))) -> List $ Doc opts
    printSubm (instanceName, subMsIdx, msIdx) = do
      let pre : Doc opts = line (index msIdx $ toVect $ allModuleNames pms) <++> line instanceName

      let inputs  = List.allFins (index ms $ index' subMs.asList subMsIdx).inpsCount <&> toTotalInputsIdx subMsIdx
      let outputs = List.allFins (index ms $ index' subMs.asList subMsIdx).outsCount <&> toTotalOutputsIdx subMsIdx

      let siNames = map (findSIName mcs mcsNames) inputs
      let soNames = map (findSOName mcs mcsNames) outputs

      let ctxInps = map (findSISVT mcs) inputs
      let ctxOuts = map (findSOSVT mcs) outputs

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
    (mcs : MultiConnectionsList ms m subMs) ->
    (sdAssigns : FinsList $ length mcs) ->
    (sdExprs : TMPExList mcs sdAssigns) ->
    (mdAssigns : FinsList $ length mcs) ->
    (mdExprs : TMPExList mcs mdAssigns) ->
    (cont : ExtendedModules $ m::ms) ->
    ExtendedModules ms


resolveInputNames : {m : _} -> (mcs : MultiConnectionsList ms m subMs) -> Vect (length mcs) String -> Vect (m.inpsCount) String
resolveInputNames mcs mcsNames = map (findTIName mcs mcsNames) $ allFins (m.inpsCount)

resolveOutputNames : {m : _} -> (mcs : MultiConnectionsList ms m subMs) -> Vect (length mcs) String -> Vect (m.outsCount) String
resolveOutputNames mcs mcsNames = map (findTOName mcs mcsNames) $ allFins (m.outsCount)

||| > In the absence of an explicit declaration, an implicit net of default net type shall be assumed
||| IEEE 1800-2023
|||
||| The default net type is wire. It could be changed to another net type using `default_nettype` directive.
||| Net types aren't compatible with unpacked arrays. So connections to unpacked array ports must be declared explicitly.
unpackedDecls : (mcs : MultiConnectionsList ms m subMs) -> Vect (length mcs) String -> List String
unpackedDecls []          _             = []
unpackedDecls (mc@(MkMC Nothing ssk Nothing ssc) :: mcs) (name::names) = if (isUnpacked $ typeOf mc) 
  then (showSVObj (typeOf mc) name) :: unpackedDecls mcs names 
  else unpackedDecls mcs names
unpackedDecls (mc :: mcs) (name::names) = unpackedDecls mcs names

export
prettyModules : {opts : _} -> {ms : _} -> Fuel ->
                (pms : PrintableModules ms) -> UniqNames ms.length (allModuleNames pms) => ExtendedModules ms -> Gen0 $ Doc opts
prettyModules x _         End = pure $ empty
prettyModules x pms @{un} (NewCompositeModule m subMs mcs sdAssigns sdExprs mdAssigns mdExprs cont) = do
  -- Generate submodule name
  (name ** isnew) <- rawNewName x @{namesGen'} (allModuleNames pms) un

  -- Generate connections names
  (mcsNames ** namesWihMcs ** unm) <- genNUniqueNamesVect x (length mcs) (allModuleNames pms) un

  -- Generate submodule instance names
  (subMInstanceNames ** namesWithSubMs ** uniosub) <- genNUniqueNamesVect x subMs.length namesWihMcs unm

  -- Resolve assigns
  let sdAssignments = printAssigns $ zip (getNames mcsNames sdAssigns) $ printTMPExprs mcsNames sdExprs
  let mdAssignments = printAssigns $ zip (getNames mcsNames mdAssigns) $ printTMPExprs mcsNames mdExprs

  -- Helper lists
  let inputNames  = resolveInputNames mcs mcsNames
  let outputNames = resolveOutputNames mcs mcsNames

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
      ((unpackedDecls mcs mcsNames) <&> \(unp) : String => line unp <+> symbol ';') ++ [ line "" ] ++
      printSubmodules m ms subMs pms mcs mcsNames (toList subMInstanceNames) (withIndex subMs.asList) ++
      [ line "", line "// Single-driven assigns" ] ++ (map line sdAssignments) ++
      [ line "", line "// Multi-driven assigns" ] ++ (map line mdAssignments)
    , line ""
    , recur
    ]
