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
  
-- public export
-- connFwdRel : {srcs, sk : SVObjList} -> {mfs : MFinsList sk.length srcs.length} -> (cons: Connections srcs sk b mfs) -> 
--              Vect (sk.length) $ Maybe $ Fin srcs.length
-- connFwdRel Empty           = []
-- connFwdRel (Cons sfs cs) = helper sfs :: connFwdRel cs where
--   helper : SourceForSink srcs sink mf -> Maybe $ Fin srcs.length
--   helper NoSource             = Nothing
--   helper (HasSource srcIdx _) = Just srcIdx

nothings : Vect sk (Maybe a) -> Nat
nothings []              = 0
nothings (Nothing :: xs) = S (nothings xs)
nothings (Just _  :: xs) = nothings xs

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

concatInpsOuts: {opts : _} -> List String -> List String -> Doc opts
concatInpsOuts inputs outputs = (tuple $ line <$> outputs ++ inputs) <+> symbol ';'

public export
allModuleNames : PrintableModules ms -> SVect ms.length
allModuleNames []        = []
allModuleNames (x :: xs) = x.name :: allModuleNames xs

printConnections: String -> (cons: SVObjList) -> Vect (cons.length) String -> List String
printConnections keyword cons names = zipWith (\conn, name => "\{keyword} \{showSVObj conn name}") (toList cons) (toList names)

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

-- ||| > In the absence of an explicit declaration, an implicit net of default net type shall be assumed
-- ||| IEEE 1800-2023
-- |||
-- ||| The default net type is wire. It could be changed to another net type using `default_nettype` directive.
-- ||| Net types aren't compatible with unpacked arrays. So connections to unpacked array ports must be declared explicitly.
-- |||
-- ||| Prints an explicit declaration for each submodule input that's not connected to any source
-- resolveUnpSI : Vect sk String -> List ((Fin sk, Maybe a), SVObject) -> List String
-- resolveUnpSI names = mapMaybe resolve' where
--   resolve' : ((Fin sk, Maybe a), SVObject) -> Maybe String
--   resolve' (x, svobj) with (valueOf svobj)
--     resolve' ((finSK, Nothing), svobj) | (UnpackedArr _ _ _) = Just $ showSVObj svobj $ index finSK names
--     resolve' (_, _)                    | _                   = Nothing

-- ||| Prints an explicit declaration for each submodule output connected to a submodule input or not connected at all.
-- ||| Doesn't print declaration for ports connected to top outputs
-- resolveUnpSO : Foldable c => Foldable d => c String -> d (String, SVObject) -> List String
-- resolveUnpSO tops = flip foldr [] $ \case
--   (n, u@(Net nt $ UnpackedArr t s e)) => if elem n tops then id else with Prelude.(::) (showSVObj u n ::)
--   (n, u@(Var    $ UnpackedArr t s e)) => if elem n tops then id else with Prelude.(::) (showSVObj u n ::)
--   _ => id {a=List String}

-- ||| filter `top inputs -> top outputs` connections
-- filterTITO : Vect n (Maybe $ Fin ss) -> (inps : Nat) -> Vect n $ Maybe $ Fin inps
-- filterTITO []        _    = []
-- filterTITO (x :: xs) inps = tryToFit' x :: filterTITO xs inps where
--   tryToFit' : Maybe (Fin from) -> Maybe $ Fin inps
--   tryToFit' Nothing    = Nothing
--   tryToFit' (Just fin) = tryToFit fin

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
-- zipPLWList : Foldable b => b a -> SVObjList -> List (a, SVObject)
-- zipPLWList other ports = toList other `zip` toList ports

printLiterals : LiteralsList ls -> List String
printLiterals []        = []
printLiterals (b :: xs) = show b :: printLiterals xs

getNames : Vect l String -> List (Fin l) -> List String
getNames names []        = []
getNames names (x :: xs) = index x names :: getNames names xs

-- typeOf' : List SVObject -> Fin a -> Maybe SVObject -- UNSAFE
-- typeOf' []      _      = Nothing
-- typeOf' (x::xs) FZ     = Just x
-- typeOf' (x::xs) (FS i) = typeOf' xs i

-- allInputs' : ModuleSigsList -> List SVObject
-- allInputs' []      = []
-- allInputs' (m::ms) = (toList m.inputs) ++ allInputs' ms

-- allOutputs' : ModuleSigsList -> List SVObject
-- allOutputs' []      = []
-- allOutputs' (m::ms) = (toList m.outputs) ++ allOutputs' ms

isElem : Eq a => (x : a) -> (xs : List a) -> Bool
isElem x []      = False
isElem x (y::xs) = case x == y of
  True => True
  False => isElem x xs

-- findSubInps : MultiConnection (SC ms m subMs sicons tocons) -> Fin (totalInputs {ms} subMs) -> Maybe SVObject
-- findSubInps (MC subInps topOuts source type) f = case isElem f subInps of
--   True  => Just type
--   False => Nothing

-- findSubOuts : MultiConnection (SC ms m subMs sicons tocons) -> Fin (totalInputs {ms} subMs) -> Maybe SVObject
-- findSubOuts (MC subInps topOuts source type) f = case isElem f subInps of
--   True  => Just type
--   False => Nothing

-- findPort : {ms:_} -> {subMs:_} -> {sicons : MFinsList (totalInputs {ms} subMs) $ allSrcsLen m ms subMs} ->
--            {tocons : MFinsList (m.outsCount) $ allSrcsLen m ms subMs} -> 
--            MultiConnectionsVect mcsl (SC ms m subMs sicons tocons) -> Fin (totalInputs {ms} subMs) -> Maybe SVObject
-- findPort []      f = Nothing
-- findPort (x::xs) f = case ?actuallyFind x f of
--   Nothing  => findPort xs f
--   Just res => Just res
--   -- where

iMcsByF : (mcs : MultiConnectionsList ms m subMs) ->
          (extractField : MultiConnection ms m subMs -> List $ Fin iport) -> Fin iport -> Maybe (Fin $ length mcs)
iMcsByF mcs func fin = findIndex resolve $ toVect mcs where
    resolve : MultiConnection ms m subMs -> Bool
    resolve sc = isElem fin $ func sc

findMcsNameByF : (mcs : MultiConnectionsList ms m subMs) -> Vect (length mcs) String -> 
                 (extractField : MultiConnection ms m subMs -> List $ Fin iport) -> Fin iport -> Maybe String
findMcsNameByF mcs mcsNames func fin = case iMcsByF mcs func fin of
  Just mcsFin => Just $ index mcsFin mcsNames
  Nothing     => Nothing

-- handleRefs : FinsList n -> List (Fin $ length $ allInputs subMs)))
-- handleRefs ACCNil                   = []
-- handleRefs (ACCOne  {use} i)        = [ lookUp use i ]
-- handleRefs (ACCCons {use} i _ rest) = lookUp use i :: handleRefs rest

actuallyFindSubSnks : MultiConnection ms m subMs -> List (Fin $ subSnks' ms m subMs)
actuallyFindSubSnks (MkMC _ ssk _ _) = ssk.asList

findSubPortName : (mcs : MultiConnectionsList ms m subMs) -> (mcsNames : Vect (length mcs) String) -> 
                  (MultiConnection ms m subMs -> List $ Fin subs) -> Fin subs -> String
findSubPortName mcs mcsNames func f = case findMcsNameByF mcs mcsNames func f of
  Just name => name
  Nothing   => "error!!"

findSIName : (mcs : MultiConnectionsList ms m subMs) -> (mcsNames : Vect (length mcs) String) -> Fin (subSnks' ms m subMs) -> String
findSIName mcs mcsNames f = findSubPortName mcs mcsNames actuallyFindSubSnks f

actuallyFindSubSrcs : MultiConnection ms m subMs -> List (Fin $ subSrcs' ms m subMs)
actuallyFindSubSrcs (MkMC _ _ _ ssc) = ssc.asList

findSOName : (mcs : MultiConnectionsList ms m subMs) -> (mcsNames : Vect (length mcs) String) -> Fin (subSrcs' ms m subMs) -> String
findSOName mcs mcsNames f = findSubPortName mcs mcsNames actuallyFindSubSrcs f

findSubPortType : MultiConnectionsList ms m subMs -> 
                  (MultiConnection ms m subMs -> List $ Fin $ subs) -> Fin subs -> SVObject
findSubPortType mcs func fin = case iMcsByF mcs func fin of
  Just mcsFin => typeOf $ index mcs mcsFin
  Nothing     => defaultNetType -- error. should not be possible

findSISVT : MultiConnectionsList ms m subMs -> Fin (subSnks' ms m subMs) -> SVObject
findSISVT mcs fin = findSubPortType mcs actuallyFindSubSnks fin

findSOSVT : MultiConnectionsList ms m subMs -> Fin (subSrcs' ms m subMs) -> SVObject
findSOSVT mcs fin = findSubPortType mcs actuallyFindSubSrcs fin


parameters {opts : LayoutOpts} (m : ModuleSig) (ms: ModuleSigsList)  (subMs : FinsList ms.length) (pms : PrintableModules ms)
          --  (subMINames : Vect (totalInputs {ms} subMs) String) (subMONames : Vect (length $ allOutputs {ms} subMs) String)
          --  (ports : ModuleSigsList) {mcsl : Nat} 
          --  {sicons : MFinsList (totalInputs {ms} subMs) $ allSrcsLen m ms subMs}
          --  {tocons : MFinsList (m.outsCount) $ allSrcsLen m ms subMs}
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

      -- let ctxInps' = catMaybes $ map (\x => findPort mcs x) inputs

      let ctxInps = map (findSISVT mcs) inputs
      let ctxOuts = map (findSOSVT mcs) outputs

      -- ?dsadsa

      let modulePrintable = index pms msIdx
      case modulePrintable.insOuts of
        StdModule  _      _      => printSubm' (pre <+> concatInpsOuts siNames soNames) siNames soNames (index ms msIdx) ctxInps ctxOuts siNames soNames
        UserModule exInps exOuts => do
          let inpsJoined = nameBasedConnections (toList exInps) siNames
          let outsJoined = nameBasedConnections (toList exOuts) soNames

          printSubm' (pre <+> concatInpsOuts inpsJoined outsJoined) siNames soNames (index ms msIdx) ctxInps ctxOuts (toList exInps) (toList exOuts)

-- giveNamesMCS : {ms : ModuleSigsList} ->
--                {m : ModuleSig} ->
--                {subMs : FinsList ms.length} ->
--                {sicons : MFinsList (totalInputs {ms} subMs) $ allSrcsLen m ms subMs} ->
--                {tocons : MFinsList (m.outsCount)            $ allSrcsLen m ms subMs} ->
--                Vect (totalInputs {ms} subMs) String ->
--                Vect (m.outsCount) String ->
--                Vect (allSrcsLen m ms subMs) String ->
--                MultiConnectionsVect mcsl (SC ms m subMs sicons tocons) -> Vect mcsl String
-- giveNamesMCS _       _       _        []      = []
-- giveNamesMCS siNames toNames srcNames (x::xs) = giveName x :: giveNamesMCS siNames toNames srcNames xs where
--   giveName : MultiConnection (SC ms m subMs sicons tocons) -> String
--   giveName (MC []      Nothing   Nothing _)   = "impossible case"
--   giveName (MC (si::_) Nothing   Nothing _)   = index si siNames
--   giveName (MC subInps (Just ts) Nothing _)   = index ts toNames
--   giveName (MC subInps topOuts   (Just ss) _) = index ss srcNames

public export
data ExtendedModules : ModuleSigsList -> Type where

  End : ExtendedModules ms
  ||| A module with assigns and literals
  NewCompositeModule :
    (m : ModuleSig) ->
    (subMs : FinsList ms.length) ->
    (mcs : MultiConnectionsList ms m subMs) ->
    -- {sicons : MFinsList (totalInputs {ms} subMs) $ allSrcsLen m ms subMs} ->
    -- {tocons : MFinsList (m.outsCount) $ allSrcsLen m ms subMs} ->
    -- (sssi : Connections (allSrcs m ms subMs) (allInputs {ms} subMs) SubInps sicons) ->
    -- (ssto : Connections (allSrcs m ms subMs) (m.outputs)            TopOuts tocons) ->
    -- {mcsl : Nat} ->
    -- (mcs : MultiConnectionsVect mcsl $ SC ms m subMs sicons tocons) ->
    (sdAssigns : List $ Fin $ length mcs) ->
    {spl : SVObjList} ->
    (sdLiterals : LiteralsList spl) ->
    (mdAssigns : List $ Fin $ length mcs) ->
    {mpl : SVObjList} ->
    (mdLiterals : LiteralsList mpl) ->
    -- (ports : ModuleSigsList) ->
    (cont : ExtendedModules $ m::ms) ->
    ExtendedModules ms

findTIName : (mcs : MultiConnectionsList ms m subMs) -> Vect (length mcs) String -> Fin (m.inpsCount) -> Maybe String
findTIName mcs mcsNames f = findMcsNameByF mcs mcsNames actuallyFind f where
  actuallyFind : MultiConnection ms m subMs -> List (Fin m.inpsCount)
  actuallyFind (MkMC _ _ (Just i) _) = [ i ]
  actuallyFind _                     = []

resolveInputNames : {m : _} -> (mcs : MultiConnectionsList ms m subMs) -> Vect (length mcs) String -> Vect (m.inpsCount) String
resolveInputNames mcs mcsNames = map resolve $ allFins (m.inpsCount) where
  resolve : Fin (m.inpsCount) -> String
  resolve f = case findTIName mcs mcsNames f of
    Just name => name
    Nothing => "error!!"

findTOName : (mcs : MultiConnectionsList ms m subMs) -> Vect (length mcs) String -> Fin (m.outsCount) -> Maybe String
findTOName mcs mcsNames f = findMcsNameByF mcs mcsNames actuallyFind f where
  actuallyFind : MultiConnection ms m subMs -> List (Fin m.outsCount)
  actuallyFind (MkMC (Just i) _ _ _) = [ i ]
  actuallyFind _                     = []

resolveOutputNames : {m : _} -> (mcs : MultiConnectionsList ms m subMs) -> Vect (length mcs) String -> Vect (m.outsCount) String
resolveOutputNames mcs mcsNames = map resolve $ allFins (m.outsCount) where
  resolve : Fin (m.outsCount) -> String
  resolve f = case findTOName mcs mcsNames f of
    Just name => name
    Nothing => "error!!"

unpackedDecls : (mcs : MultiConnectionsList ms m subMs) -> Vect (length mcs) String -> List String
unpackedDecls []          _             = []
unpackedDecls (mc@(MkMC Nothing ssk Nothing ssc) :: mcs) (name::names) = if (isUnpacked $ typeOf mc) 
  then (showSVObj (typeOf mc) name) :: unpackedDecls mcs names 
  else unpackedDecls mcs names
unpackedDecls (mc :: mcs) (name::names) = unpackedDecls mcs names

sf : Fin n -> String
sf x = show $ finToNat x

debugPrint'' : MFin n -> String
debugPrint'' Nothing = "Nothing"
debugPrint'' (Just x) = sf x

debugPrint''' : FinsList n -> String
debugPrint''' []      = "Nil"
debugPrint''' (x::xs) = "\{sf x},\{debugPrint''' xs}"

debugPrint' : MultiConnection ms m subMs -> String
debugPrint' (MkMC tsk ssk tsc ssc) = "MC{ tsk: \{debugPrint'' tsk} | ssk: \{debugPrint''' ssk} | tsc: \{debugPrint'' tsc} | ssc: \{debugPrint''' ssc} }"

export
debugPrint : MultiConnectionsList ms m subMs -> List String
debugPrint mcs = map debugPrint' $ toList $ toVect mcs

export
prettyModules : {opts : _} -> {ms : _} -> Fuel ->
                (pms : PrintableModules ms) -> UniqNames ms.length (allModuleNames pms) => ExtendedModules ms -> Gen0 $ Doc opts
prettyModules x _         End = pure $ empty -- line "yeah some shit"
prettyModules x pms @{un} (NewCompositeModule m subMs mcs sdAssigns sdLiterals mdAssigns mdLiterals cont) = do
  -- Generate submodule name
  (name ** isnew) <- rawNewName x @{namesGen'} (allModuleNames pms) un

  (mcsNames ** namesWihMcs ** unm) <- genNUniqueNamesVect x (length mcs) (allModuleNames pms) un

  -- -- Generate top module input names
  -- (inputNames ** namesWithInputs ** uni) <- genNUniqueNamesVect x m.inpsCount (allModuleNames pms) un
  -- -- Generate submodule output names
  -- (subMONames ** namesWithSubOuts ** unis) <- genNUniqueNamesVect x (allOutputs {ms} subMs).length namesWithInputs uni
  -- Generate submodule instance names
  (subMInstanceNames ** namesWithSubMs ** uniosub) <- genNUniqueNamesVect x subMs.length namesWihMcs unm

  -- -- Resolve submodule inputs
  -- let siss = connFwdRel sssi
  -- (subMINames, (namesWithNoSources ** uniosubn)) <- resolveSinks siss (comLen $ inputNames ++ subMONames) x namesWithSubMs uniosub

  -- -- Resolve top outputs
  -- let toss = connFwdRel ssto
  -- (assignedInpNames ** namesWithTIN ** uniosubnt) <- genNUniqueNamesVect x m.inpsCount namesWithNoSources uniosubn
  -- (outputNames, (namesWithNoTopOuts ** uniosubnto)) <- resolveSinks toss (comLen $ assignedInpNames ++ subMONames) x namesWithTIN uniosubnt

  -- -- Resolve `top inputs -> top outputs` connections
  -- let (_ ** tito) = catMaybes $ resolveConAssigns (filterTITO toss m.inpsCount) outputNames inputNames

  -- -- Unpacked arrays declarations
  -- let unpackedDecls = resolveUnpSI subMINames (withIndex siss `zipPLWList` allInputs {ms} subMs)
  --                  ++ resolveUnpSO outputNames (subMONames `zipPLWList` allOutputs {ms} subMs)

  -- -- Resolve mcs names
  -- let mcsNames = giveNamesMCS subMINames outputNames (comLen $ inputNames ++ subMONames) mcs
  -- Resolve assigns
  let sdAssignments = printAssigns $ zip (getNames mcsNames sdAssigns) $ printLiterals sdLiterals
  let mdAssignments = printAssigns $ zip (getNames mcsNames mdAssigns) $ printLiterals mdLiterals

  let inputNames = resolveInputNames mcs mcsNames
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
      -- [ line "", line "// Top inputs -> top outputs assigns" ] ++ (map line $ toList tito) ++
      [ line "", line "// Single-driven assigns" ] ++ (map line sdAssignments) ++
      [ line "", line "// Multi-driven assigns" ] ++ (map line mdAssignments)
    , line ""
    -- , line "allFins tsk : \{debugPrint''' $ allFins $ topSnks' m}"
    -- , line "allFins ssk : \{debugPrint''' $ allFins $ subSnks' ms m subMs}"
    -- , line "allFins tsc : \{debugPrint''' $ allFins $ topSrcs' m}"
    -- , line "allFins ssc : \{debugPrint''' $ allFins $ subSrcs' ms m subMs}"
    -- , line "tsk len: \{show $ topSnks' m}"
    -- , line "ssk len: \{show $ subSnks' ms m subMs}"
    -- , line "tsc len: \{show $ topSrcs' m}"
    -- , line "ssc len: \{show $ subSrcs' ms m subMs}"
    -- , line "mcs len: \{show $ length mcs}"
    -- , (line $ joinBy "\n" $ debugPrint mcs)
    -- , line "// recur"
    , recur
    ]
