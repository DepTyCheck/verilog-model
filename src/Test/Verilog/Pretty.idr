module Test.Verilog.Pretty

-- import Data.Either
-- import Data.List
-- import Data.List.Extra
-- import Data.List1
-- import Data.List.Lazy
-- import Data.String
-- import Data.List.Elem

-- import Data.Fin.Split
-- import Data.Fuel
-- import Data.Vect
-- import Data.Vect.Extra

-- import Data.Fin.ToFin

-- import Test.Verilog.UniqueNames.Derived
-- import Test.Verilog.PrintableModules

-- -- import Test.Verilog.SVType
-- -- import Test.Verilog.Connections
-- import Test.Verilog.Assign
-- import Test.Verilog.Literal
-- import Test.Verilog.TMPExpression
-- import Test.Verilog.Warnings
-- import Test.Verilog.SVDesign


import Data.Fuel
import Data.Vect
import Data.Vect.Extra
import Data.List
import Data.Fin
import Data.String

import Test.Common.Design
import public Test.Common.PrintableDesigns
import public Test.Common.UniqueNames
import Test.Verilog.SVDesign
import Test.Verilog.Warnings
import public Test.Verilog.UniqueNames
import Test.Verilog.SVType
import Test.Verilog.Literal
import Test.Verilog.TMPExpression

import Test.DepTyCheck.Gen
import Text.PrettyPrint.Bernardy
import Syntax.IHateParens.List

%default total

-- public export
-- toTotalInputsIdx : {ms : _} -> {subMs : FinsList ms.length} ->
--                   (idx : Fin subMs.asList.length) ->
--                   Fin (index ms (index' subMs.asList idx)).inpsCount ->
--                   Fin $ totalInputs {ms} subMs
-- toTotalInputsIdx {subMs=i::is} idx x with 0 (sym $ svolistAppendLen (index ms i).inputs (allInputs {ms} is))
--                                         | 0 (length ((index ms i).inputs ++ allInputs {ms} is))
--   toTotalInputsIdx FZ       x | Refl | _ = indexSum $ Left x
--   toTotalInputsIdx (FS idx) x | Refl | _ = indexSum $ Right $ toTotalInputsIdx idx x

-- public export
-- toTotalOutputsIdx : {ms : _} -> {subMs : FinsList ms.length} ->
--                     (idx : Fin subMs.asList.length) ->
--                     Fin (index ms $ index' subMs.asList idx).outsCount ->
--                     Fin $ totalOutputs {ms} subMs
-- toTotalOutputsIdx {subMs=i::is} idx x with 0 (sym $ svolistAppendLen (index ms i).outputs (allOutputs {ms} is))
--                                          | 0 (length ((index ms i).outputs ++ allOutputs {ms} is))
--   toTotalOutputsIdx FZ       x | Refl | _ = indexSum $ Left x
--   toTotalOutputsIdx (FS idx) x | Refl | _ = indexSum $ Right $ toTotalOutputsIdx idx x

Show SVPortMode where
  show In    = "input"
  show Out   = "output"
  show InOut = "inout"
  -- show Ref   = "ref"

Show (PortMode SystemVerilog) where
  show (SVP x) = show x

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

Show Real where
  show Shortreal' = "shortreal"
  show Real'      = "real"
  show Realtime'  = "realtime"

Show Atom where
  show Bit'   = "bit"
  show Logic' = "logic"
  show Reg'   = "reg"

Show Vector where
  show Byte'     = "byte"
  show Shortint' = "shortint"
  show Int'      = "int"
  show Longint'  = "longint"
  show Integer'  = "integer"
  show Time'     = "time"

Show (Binary s) where
  show S = "1"
  show Z = "0"
  show X = "x"
  show H = "z"

Show (BinaryList s) where
  show bl = "'b\{showLinear bl}" where
    showLinear : BinaryList s -> String
    showLinear (One  x)    = show x
    showLinear (More x xs) = show x ++ showLinear xs

Show (TypeLiteralVect l t)

||| Single bit example:
||| logic m;
||| assign m = 'b1;
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
  show (AL  x) = show x
  show (VL  x) = show x
  show (PAL x) = show x
  show (UAL x) = show x

Show (TypeLiteralVect l t) where
  show x = "'{\{joinBy "," $ map show $ toList x}}"

||| print name
pn : String -> String
pn "" = ""
pn a = " \{a}"

showBasic : SVType -> String
showBasic (RVar x)            = show x
showBasic (AVar x)            = show x
showBasic (VVar x)            = show x
showBasic (PackedArr   t k j) = showBasic t
showBasic (UnpackedArr t k j) = showBasic t

showPackedSVT : SVType -> String
showPackedSVT (RVar x)              = show x
showPackedSVT (AVar x)              = show x
showPackedSVT (VVar x)              = show x
showPackedSVT (PackedArr t {p} s e) = "\{showBasic t} [\{show s}:\{show e}]\{packDims t}" where
  packDims : SVType -> String
  packDims (PackedArr t s e) = "[\{show s}:\{show e}]" ++ packDims t
  packDims _                 = ""
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
showSVType rv@(RVar x)                name = "\{show x}\{pn name}"
showSVType sv@(AVar x)                name = "\{show x}\{pn name}"
showSVType vv@(VVar x)                name = "\{show x}\{pn name}"
showSVType pa@(PackedArr   t {p} s e) name = "\{showPackedSVT pa}\{pn name}"
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

nameBasedConnections : List String -> List String -> List String
nameBasedConnections = zipWith $ \external, internal => ".\{external}(\{internal})"

concatInpsOuts : {opts : _} -> List String -> List String -> Doc opts
concatInpsOuts inputs outputs = (tuple $ line <$> outputs ++ inputs) <+> symbol ';'

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

parameters {opts : LayoutOpts}
           (s : DesignUnitSig SystemVerilog) {usl : _} {subUs : _}
           (moduleName : String) (topNames : Vect (s.portsCnt) String) (subEntNames : Vect (subUs.length) String)
           (mcs : MultiConnectionsList SystemVerilog s usl subUs) (mcsNames : Vect (length mcs) String)
           (prevEntNames : SVect $ usl.length) (pds : PrintableDesigns SystemVerilog usl)

  subModuleTypeName : Fin subUs.length -> String
  subModuleTypeName subFin = index (index subUs subFin) $ toVect prevEntNames

  ||| Flat indices
  subPortFins : Fin subUs.length -> List (Fin (totalSubs' usl subUs))
  subPortFins subFin = map (toTotalSubsIdx subFin) $ List.allFins (index usl $ index subUs subFin).portsCnt

  subPortSigNames : Fin subUs.length -> List String
  subPortSigNames = map (findSubPortName {s} {mcs} {mcsNames}) . subPortFins

  ||| .portName(sigName)
  namedConn : String -> String -> Doc opts
  namedConn portName sigName = line ".\{portName}(\{sigName})"

  subConnections : List String -> Fin subUs.length -> List $ Doc opts
  subConnections sigNames subFin = case (index pds $ index subUs subFin).portNames of
    StdModule  _     => map line sigNames
    UserModule ports => zipWith namedConn (toList ports) sigNames

  ||| Actual SVObject type of the signal connected to a sub-module port
  findSubPortMcType : Fin (totalSubs' usl subUs) -> SVObject
  findSubPortMcType f = case isSubPortOf f mcs of
    Nothing   => Var (AVar Logic')
    Just mcsF => dtToSVt $ typeOf $ index mcs mcsF

  ||| Formal sub port types
  subPortFormalTypes : Fin subUs.length -> List SVObject
  subPortFormalTypes = map (dtToSVt . (.type) . subPort usl subUs) . subPortFins

  ||| Actual sub port types
  subPortActualTypes : Fin subUs.length -> List SVObject
  subPortActualTypes = map findSubPortMcType . subPortFins

  subPortFormalNames : Fin subUs.length -> List String
  subPortFormalNames subFin = case (index pds $ index subUs subFin).portNames of
    StdModule  _     => subPortSigNames subFin
    UserModule ports => toList ports

  subModuleWarningDocs : Fin subUs.length -> List $ Doc opts
  subModuleWarningDocs subFin =
    let sigNames    = subPortSigNames subFin
        formalNames = subPortFormalNames subFin
    in map line $ printAllImplicitCasts showSVObj
         (subPortActualTypes subFin) sigNames      -- actual signal: what is connected
         (subPortFormalTypes subFin) formalNames   -- formal port:   what the sub-module declares

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
  --   <type-name> <instance-name> (<port-connections>);
  subModuleDecl : (String, Fin subUs.length) -> Doc opts
  subModuleDecl (instanceName, subFin) =
    let sigNames = subPortSigNames subFin
        conns    = subConnections sigNames subFin
        warnings = subModuleWarningDocs subFin
        header   = line "\{subModuleTypeName subFin} \{instanceName}" <+>
                   generalList (line "(") (line ");") comma conns
    in vsep $ header :: warnings ++ [emptyLine]

  ||| > In the absence of an explicit declaration, an implicit net of default net type shall be assumed
  ||| IEEE 1800-2023
  |||
  ||| The default net type is wire. It could be changed to another net type using `default_nettype` directive.
  ||| Net types aren't compatible with unpacked arrays. So connections to unpacked array ports must be declared explicitly.
  unpackedDecl : (mc : MultiConnection SystemVerilog s usl subUs) -> String -> Maybe (Doc opts)
  unpackedDecl mc@(MkMC Nothing Nothing _ _ @{_} @{NoTop}) name =
    case isUnpacked (dtToSVt $ typeOf mc) of
      True  => Just $ line "\{showSVObj (dtToSVt $ typeOf mc) name};"
      False => Nothing
  unpackedDecl _ _ = Nothing

  unpackedDeclDocs : List $ Doc opts
  unpackedDeclDocs = catMaybes $ toList $ zipWith unpackedDecl (toVect mcs) mcsNames

  unpackedSection : Doc opts
  unpackedSection = case unpackedDeclDocs of
    [] => empty
    ds => vsep $ line "// Unpacked net declarations" :: ds ++ [emptyLine]

  assignSection : String -> List String -> List $ Doc opts
  assignSection comment assigns = line comment :: map line assigns

  context : (sdAssignments : List String) -> (mdAssignments : List String) -> Gen0 $ Doc opts
  context sdAssignments mdAssignments = pure $ vsep
    [
      unpackedSection
    , vsep $ map subModuleDecl (toList $ zip subEntNames $ Vect.allFins subUs.length)
    , emptyLine
    , vsep $ assignSection "// Single-driven assigns" sdAssignments
    , emptyLine
    , vsep $ assignSection "// Multi-driven assigns"  mdAssignments
    ]


  topPorts : Gen0 $ List $ Doc opts
  topPorts = forEachTopPort mcs topNames print where
    print : DataType SystemVerilog -> String -> PortMode SystemVerilog -> Gen MaybeEmpty (Doc opts)
    print (SVT obj) name mode = pure $ line "\{show mode} \{showSVObj obj name}"

  printModule : (sdAssignments : List String) -> (mdAssignments : List String) -> Gen0 $ Doc opts
  printModule sdAssignments mdAssignments = do
    tp <- topPorts
    ctx <- context sdAssignments mdAssignments
    pure $ vsep
      [
        line "module \{moduleName} " <+> generalList (line "(") (line ");") comma tp
      , defaultIndent ctx
      , line "endmodule: \{moduleName}"
      ]

export
prettyModules : {opts : _} -> {dus : _} -> Fuel ->
               (pds : PrintableDesigns SystemVerilog dus) -> UniqNames dus.length (allDesignNames pds) => SVDesign dus -> Gen0 $ Doc opts
prettyModules _ _         End                                                                          = pure empty
prettyModules x pds @{un} (New {s} {usl} {subUs} {mcs} basic sdAssigns sdExprs mdAssigns mdExprs cont) = do
  (moduleName ** connNames ** subEntNames ** unDes ** unEntConnSub) <- genPDNames VerilogKeywords x pds {un} basic
  let allEntConSubNames : ?
      allEntConSubNames = subEntNames ++ connNames ++ moduleName :: allDesignNames pds
  let mcsNames : ?
      mcsNames = toVect connNames
  let subEntNamesVect : ?
      subEntNamesVect = toVect subEntNames

  -- Resolve assigns
  let sdAssignments = printAssigns $ zip (getNames mcsNames sdAssigns) $ printTMPExprs mcsNames sdExprs
  let mdAssignments = printAssigns $ zip (getNames mcsNames mdAssigns) $ printTMPExprs mcsNames mdExprs

  -- Resolve names
  let topNames = resolveInpsOutsNames basic mcsNames
  let generatedPrintableInfo : ?
      generatedPrintableInfo = MkPrintableDesign moduleName (UserModule topNames)

  -- Recursive call to use at the end
  recur <- prettyModules {opts} x (generatedPrintableInfo :: pds) @{unDes} cont
  cur <- printModule s moduleName topNames subEntNamesVect mcs mcsNames (allDesignNames pds) pds sdAssignments mdAssignments
  pure $ vsep
    [
      cur
    , emptyLine
    , recur
    ]
