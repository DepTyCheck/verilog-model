module Test.VHDL.Pretty

import Data.Fuel
import Data.Vect
import Data.Vect.Extra
import Data.List
import Data.Fin
import Data.String

import Test.Common.Design
import public Test.Common.PrintableDesigns
import public Test.Common.UniqueNames
import Test.VHDL.Design
import public Test.VHDL.UniqueNames
import Test.VHDL.Type

import Test.DepTyCheck.Gen
import Text.PrettyPrint.Bernardy

%default total

vtypeOf : {s : _} -> {usl : _} -> {subUs : _} -> MultiConnection VHDL s usl subUs -> VHDLType
vtypeOf = valueOf . dtToVHt . typeOf

Show PredefinedArrayTypes where
  show BOOLEAN_VECTOR = "boolean_vector"
  show BIT_VECTOR     = "bit_vector"
  show INTEGER_VECTOR = "integer_vector"
  show REAL_VECTOR    = "real_vector"
  show TIME_VECTOR    = "time_vector"
  show STRING         = "string"

Show ArrayDirection where
  show Up     = "to"
  show Down = "downto"

Show (Dimension t) where
  show (MkDim start end direction) = "(\{show start} \{show direction} \{show end})"

printType : (type : VHDLType) -> Gen0 String
printType (Enum CHARACTER)      = pure $ "character"
printType (Enum BIT)            = pure $ "bit"
printType (Enum BOOLEAN)        = pure $ "boolean"
printType (Enum SEVERITY_LEVEL) = pure $ "severity_level"
printType Integer'              = pure $ "integer"
printType Physical              = pure $ "time"
printType Real                  = pure $ "real"
printType StdLogic              = pure $ "std_logic"
printType (Array t d)           = pure $ "\{show t}\{show d}"
printType (StdLogicVector d)    = pure $ "std_logic_vector\{show d}"

printPort : (name : String) -> (dir : String) -> (type : VHDLObject) -> Gen0 String
printPort name dir t = do
  ts <- printType $ valueOf t
  pure $ "\{name} : \{dir} \{ts}"

||| Library header followed by a separating empty line, or nothing at all.
||| Returning a list lets callers append it without leaving a stray blank line
||| when the header is absent.
libHeaderSection : {opts : _} -> Bool -> List (Doc opts)
libHeaderSection True = [
      line "library ieee;"
    , line "use ieee.std_logic_1164.all;"
    , emptyLine
    ]
libHeaderSection False = []

||| Creates the formal => actual association
||| formal: the name inside the sub-component
||| actual: the signal name in the current architecture
assoc : {opts : _} -> String -> String -> Doc opts
assoc formal actual = line "\{formal} => \{actual}"

Show VHDLPortMode where
  show In      = "in"
  show Out     = "out"
  show InOut   = "inout"
  show Buffer  = "buffer"
  show Linkage = "linkage"

Show (PortMode VHDL) where
  show (VHP x) = show x

Show BinValue where
  show Zero = "0"
  show One  = "1"

Show OctValue where
  show Zero  = "0"
  show One   = "1"
  show Two   = "2"
  show Three = "3"
  show Four  = "4"
  show Five  = "5"
  show Six   = "6"
  show Seven = "7"

Show HexValue where
  show Zero  = "0"
  show One   = "1"
  show Two   = "2"
  show Three = "3"
  show Four  = "4"
  show Five  = "5"
  show Six   = "6"
  show Seven = "7"
  show Eight = "8"
  show Nine  = "9"
  show A     = "A"
  show B     = "B"
  show C     = "C"
  show D     = "D"
  show E     = "E"
  show F     = "F"

Show BoolValue where
  show True  = "TRUE"
  show False = "FALSE"

printBoolLit : BoolValue -> Gen0 String
printBoolLit b = pure $ show b

Show SeverityLevelValue where
  show NOTE    = "NOTE"
  show WARNING = "WARNING"
  show ERROR   = "ERROR"
  show FAILURE = "FAILURE"

Show NumBase where
  show Binary      = "2"
  show Octal       = "8"
  show Hexadecimal = "16"

Show (BaseValue b) where
  show (MkBin x) = show x
  show (MkOct x) = show x
  show (MkHex x) = show x

data IntegerDisplay = Usual | Underline

genIntegerDisplay : Gen0 IntegerDisplay
genIntegerDisplay = elements [ Usual, Underline ]

integerSep : IntegerDisplay -> String
integerSep Usual     = ""
integerSep Underline = "_"

||| 123456 or 123_456 or FFF_FFF
printLiteralBody : (a -> String) -> List a -> Gen0 String
printLiteralBody f items = do
  d <- genIntegerDisplay
  pure $ joinBy (integerSep d) $ map f $ toList items

printUniversalInteger : UniversalInteger -> Gen0 String
printUniversalInteger (MkInt ns)    = printLiteralBody show $ toList ns
printUniversalInteger (MkBInt b bs) = do
  body <- printLiteralBody show $ toList bs
  pure $ "\{show b}#\{body}#"

printIndependentNumLiteral : Fuel -> a -> (a -> Gen0 String) -> Gen0 String
printIndependentNumLiteral x num print = do
  numStr <- print num
  -- e <- genExponent x
  pure $ numStr -- ++ show e

printIndependentInteger : Fuel -> UniversalInteger -> Gen0 String
printIndependentInteger x i = printIndependentNumLiteral x i printUniversalInteger

Show TimeUnits where
  show Fs  = "fs"
  show Ps  = "ps"
  show Ns  = "ns"
  show Us  = "us"
  show Ms  = "ms"
  show Sec = "sec"
  show Min = "min"
  show Hr  = "hr"

printUniversalReal : UniversalReal -> Gen0 String
printUniversalReal (MkReal whole fract)    = do
  wstr <- printLiteralBody show $ toList whole
  fstr <- printLiteralBody show $ toList fract
  pure "\{wstr}.\{fstr}"
printUniversalReal (MkBReal n whole fract) = do
  wstr <- printLiteralBody show $ toList whole
  fstr <- printLiteralBody show $ toList fract
  pure "\{show n}#\{wstr}.\{fstr}#"

printIndependentReal : Fuel -> UniversalReal -> Gen0 String
printIndependentReal x ureal = printIndependentNumLiteral x ureal printUniversalReal

printTimeValue : Fuel -> TimeValue u -> Gen0 String
printTimeValue x (FU r)   = printIndependentReal x r
printTimeValue x (FI i)   = printIndependentInteger x i
printTimeValue x (PU r)   = printIndependentReal x r
printTimeValue x (PI i)   = printIndependentInteger x i
printTimeValue x (NU r)   = printIndependentReal x r
printTimeValue x (NI i)   = printIndependentInteger x i
printTimeValue x (UU r)   = printIndependentReal x r
printTimeValue x (UI i)   = printIndependentInteger x i
printTimeValue x (MU r)   = printIndependentReal x r
printTimeValue x (MI i)   = printIndependentInteger x i
printTimeValue _ (SN n _) = pure $ show n
printTimeValue _ (MN n _) = pure $ show n
printTimeValue _ (HN n _) = pure $ show n

printTimeLit : Fuel -> TimeLiteral -> Gen0 String
printTimeLit x (TL u tv) = do
  vStr <- printTimeValue x tv
  pure "\{vStr} \{show u}"

printAnyVect : (othersStub : String) -> (a -> Gen0 String) -> Vect n a -> Gen0 String
-- 9.3.3 Aggregates
-- 9.3.3.1 General
-- aggregate ::= ( element_association { , element_association } )
-- IEEE 1076-2019
--
-- So BNF forbids empty aggregate.
printAnyVect othersStub _     []        = pure "(others => \{othersStub})"
-- 9.3.3 Aggregates
-- 9.3.3.1 General
-- An aggregate is a basic operation (see 5.1) that combines one or more values into a composite value of a record or array type.
-- IEEE 1076-2019
--
-- So aggregate allows one element, but there is an ambiguity with parenthesized expression.
--
-- 9.3.3 Aggregates
-- 9.3.3.1 General
-- Aggregates containing a single element association shall always be specified
-- using named association in order to distinguish them from parenthesized expressions.
-- IEEE 1076-2019
--
-- For single element aggregates use `(others => <element>)` named association to avoid the
-- parenthesized-expression ambiguity.
printAnyVect _          print [x]       = do
  item <- print x
  pure "(others => \{item})"
printAnyVect _          print v@(_::_)  = do
  items <- traverse print $ toList v
  pure "(\{joinBy ", " items})"

printChar : Char -> Gen0 String
printChar c = pure $ singleton c

printCharLit : Char -> Gen0 String
printCharLit c = pure "'\{String.singleton c}'"

printString : CharVect n -> Gen0 String
printString cv = do
  chars <- traverse printChar $ toList $ toVect cv
  pure "\"\{concat chars}\""

printCharVectLit : CharVect n -> Gen0 String
printCharVectLit cv = oneOf [ printAnyVect "' '" printCharLit $ toVect cv, printString cv ]

printBinLit : BinValue -> Gen0 String
printBinLit b = pure $ "'\{show b}'"

Show LogicValue where
  show U  = "U"
  show X  = "X"
  show F0 = "0"
  show F1 = "1"
  show Z' = "Z"
  show W  = "W"
  show L  = "L"
  show H  = "H"
  show DC = "-"

printLogicLit : LogicValue -> Gen0 String
printLogicLit v = pure $ "'\{show v}'"

||| std_logic_vector as a string literal, ex. "WL-"
printLogicString : Vect n LogicValue -> Gen0 String
printLogicString lv = pure "\"\{concat $ map show $ toList lv}\""

||| Either an aggregate or a string literal, same as for character strings.
printLogicVectLit : Vect n LogicValue -> Gen0 String
printLogicVectLit lv = oneOf [ printAnyVect "'0'" printLogicLit lv, printLogicString lv ]

printVHLit : Fuel -> VHDLLiteral t -> Gen0 String
printVHLit x (MkChar c)       = printCharLit c
printVHLit x (MkBIT b)        = printBinLit b
printVHLit x (MkBool b)       = printBoolLit b
printVHLit x (MkSevLevel l)   = pure $ show l
printVHLit x (MkInt i)        = printIndependentInteger x i
printVHLit x (MkPhys t)       = printTimeLit x t
printVHLit x (MkReal r)       = printIndependentReal x r
printVHLit x (MkString cv)    = printCharVectLit cv
printVHLit x (MkBoolVect bv)  = printAnyVect "TRUE" printBoolLit $ toVect bv
printVHLit x (MkBitVect bv)   = printAnyVect "'0'" printBinLit $ toVect bv
printVHLit x (MkIntVect iv)   = printAnyVect "0" (\i => printIndependentInteger x i) $ toVect iv
printVHLit x (MkRealVect rv)  = printAnyVect "0.0" (\r => printIndependentReal x r) $ toVect rv
printVHLit x (MkTimeVect tv)  = printAnyVect "0 ns" (\t => printTimeLit x t) $ toVect tv
printVHLit x (MkLogic l)      = printLogicLit l
printVHLit x (MkLogicVect lv) = printLogicVectLit $ toVect lv

||| Predefined names of the units of type TIME
timeUnitNames : SVect 8
timeUnitNames = [ "fs", "ps", "ns", "us", "ms", "sec", "min", "hr" ]

isTimeLit : VHDLLiteral t -> Bool
isTimeLit (MkPhys _)     = True
isTimeLit (MkTimeVect _) = True
isTimeLit _              = False

exprHasTimeLit : Expr VHDL mcs f -> Bool
exprHasTimeLit (VH $ MkLiteral lit) = isTimeLit lit

expVectHasTimeLit : ExpVect VHDL mcs fins -> Bool
expVectHasTimeLit []        = False
expVectHasTimeLit (e :: es) = exprHasTimeLit e || expVectHasTimeLit es

||| Does the design assign any Time literal?
designHasTimeLit : DesignUnit {l=VHDL} s usl subUs mcs -> Bool
designHasTimeLit (MkDesign _ _ _ _ sdExprs _ mdExprs) = expVectHasTimeLit sdExprs || expVectHasTimeLit mdExprs

||| 12.3 Visibility
||| A declaration is said to be hidden within (part of) an inner declarative region if the
||| inner region contains a homograph of this declaration; the outer declaration is then
||| hidden within the immediate scope of the inner homograph.
||| IEEE 1076-2019
|||
||| So when the design contains a Time literal we add the unit names to the reserved keywords,
||| forbidding ports (and other generated names) to hide them
vhdlKeywordsFor : DesignUnit {l=VHDL} s usl subUs mcs -> (lk : Nat ** SVect lk)
vhdlKeywordsFor design = if designHasTimeLit design
  then (_ ** VHDLKeywords ++ timeUnitNames)
  else (_ ** VHDLKeywords)

parameters (x : Fuel) {opts : LayoutOpts} (entityName : String) (archName : String)
           (s : DesignUnitSig VHDL) (topNames : Vect (s.portsCnt) String)
           {usl : _} {subUs : _} (subEntNames : Vect (subUs.length) String)
           (mcs : MultiConnectionsList VHDL s usl subUs) (mcsNames : Vect (length mcs) String)
           (prevEntNames : SVect $ usl.length) (pds : PrintableDesigns VHDL usl)
           (design : DesignUnit {l=VHDL} s usl subUs mcs)

  ||| 6.5.6.3 Port clauses
  ||| A formal port shall have an object class that is either signal or variable.
  ||| If a formal port does not explicitly specify the object class, signal is assumed.
  ||| A formal variable port shall either be of a protected type or a composite type with a subelement of a protected type.
  ||| Its mode shall be inout.
  |||
  ||| IEEE 1076-2019
  ||| port (X, Y, Cin: in Bit; Cout, Sum: out Bit);
  printPortCaluse : Gen0 $ Doc opts
  printPortCaluse with (s.portsCnt)
   printPortCaluse | 0     = empty
   printPortCaluse | (S k) = do
    tops <- forEachTopPort mcs topNames print
    pure $ defaultIndent $ vsep [
      generalList (line "port (") (line ");") semi tops
    ] where
      print : DataType VHDL -> String -> PortMode VHDL -> Gen0 (Doc opts)
      print (VHD t) name mode = do
        ps <- printPort name (show mode) t
        pure $ line ps

  -- Is mc is connection to top port?
  isMCTopPort : Fin (length mcs) -> Bool
  isMCTopPort f with (index mcs f)
    isMCTopPort f | (MkMC (Just _) Nothing _ _ @{_} @{OnlyTSC}) = True
    isMCTopPort f | (MkMC Nothing (Just _) _ _ @{_} @{OnlyTSK}) = True
    isMCTopPort f | (MkMC Nothing Nothing  _ _ @{_} @{NoTop})   = False

  isLogic : VHDLType -> Bool
  isLogic StdLogic           = True
  isLogic (StdLogicVector _) = True
  isLogic _                  = False

  portsHaveLogic : List (Fin $ length mcs) -> Bool
  portsHaveLogic fins = foldl (\b, f => if (isLogic $ vtypeOf $ index mcs f) then True else b) False fins

  ||| Multiconnections which does not represent connection to top port are treated like signals.
  filterTopOrSub : (f : Fin (length mcs) -> Bool) -> List (Fin $ length mcs)
  filterTopOrSub f = filter f $ List.allFins $ length mcs

  topPortsHaveLogic : Bool
  topPortsHaveLogic = portsHaveLogic $ filterTopOrSub isMCTopPort

  subPortsHaveLogic : Bool
  subPortsHaveLogic =  portsHaveLogic $ filterTopOrSub $ not . isMCTopPort

  ||| 3.2.2 Entity header
  ||| entity_header ::=
  |||   [ formal_generic_clause ]
  |||   [ formal_port_clause ]
  |||
  ||| IEEE 1076-2019
  printEntity : Gen0 $ Doc opts
  printEntity = do
    portClause <- printPortCaluse
    pure $ vsep $ libHeaderSection topPortsHaveLogic ++ [
      line "entity \{entityName} is"
    , portClause
    , line "end \{entityName};"
    ]

  ||| 6.4.2.3 Signal declarations
  |||
  ||| IEEE 1076-2019
  |||
  ||| ex: signal link_sig : Bit;
  signalDeclaration : Fin (length mcs) -> Gen0 $ Doc opts
  signalDeclaration mcFin = do
    let name = index mcFin mcsNames
    ts <- printType $ vtypeOf $ index mcs mcFin
    pure $ line "signal \{name} : \{ts};"

  ||| 3.3.2 Architecture declarative part
  |||
  ||| IEEE 1076-2019
  architectureDeclarative : Gen0 $ Doc opts
  architectureDeclarative = do
    signals <- traverse signalDeclaration $ filterTopOrSub $ not . isMCTopPort
    pure $ vsep [
      vsep signals
    ]

  ||| ex: e_b : entity module_b
  subEntityHeader : String -> Fin (subUs.length) -> Doc opts
  subEntityHeader name subFin = let uslFin = index subUs subFin in line "\{name} : entity work.\{index uslFin $ toVect prevEntNames}"

  connections : (subNames : List String) -> Fin (subUs.length) -> Gen0 $ List $ Doc opts
  connections subNames subFin  = case (index pds $ index subUs subFin).portNames of
    StdModule  _     => pure $ map line $ toList subNames
    UserModule ports => do
      let exNames = toList ports
      pure $ zipWith assoc exNames subNames

  ||| ex:
  ||| port map (
  |||   a => sig_a,
  |||   b => sig_b,
  |||   y => sig_y
  ||| );
  portMap : Fin (subUs.length) -> Gen0 $ Doc opts
  portMap subFin = case (index usl $ index subUs subFin).portsCnt of
    0     => empty -- no port map if entity has no ports
    (S k) => do
      let ps  = List.allFins (index usl $ index subUs subFin).portsCnt <&> toTotalSubsIdx subFin

      let subNames = map (findSubPortName {s} {mcs} {mcsNames}) ps
      conns <- connections subNames subFin

      pure $ generalList (line "port map (") (line ");") comma $ conns

  concurrentSubEntDecl : (String, Fin (subUs.length)) -> Gen0 $ Doc opts
  concurrentSubEntDecl (name, subFin) = do
    pm <- portMap subFin
    pure $ vsep [
      subEntityHeader name subFin
    , defaultIndent $ pm
    ]

  -- prints subUs
  subEntitiesPortsMap : Gen0 $ List $ Doc opts
  subEntitiesPortsMap = traverse concurrentSubEntDecl $ toList $ zip subEntNames $ Vect.allFins $ subUs.length

  printExpr : Expr VHDL mcs f -> Gen0 String
  printExpr (VH $ MkLiteral lit) = printVHLit x lit

  printAssign : Fin mcs.length -> Expr VHDL mcs t -> Gen0 $ Doc opts
  printAssign f exp = do
    expStr <- printExpr exp
    pure $ line "\{index f mcsNames} <= \{expStr};"

  ||| 3.3.3 Architecture statement part
  |||
  ||| IEEE 1076-2019
  architectureStatement : Gen0 $ Doc opts
  architectureStatement = do
    entities <- subEntitiesPortsMap
    assigns <- printAssignsSection design printAssign
    pure $ vsepSections [entities, assigns]

  ||| 3.3 Architecture bodies
  ||| architecture_body ::=
  |||   architecture identifier of entity_name is
  |||     architecture_declarative_part
  |||   begin
  |||     architecture_statement_part
  |||   end [ architecture ]
  |||
  ||| IEEE 1076-2019
  printArchitecture : Gen0 $ Doc opts
  printArchitecture = do
    archDecl <- architectureDeclarative
    archState <- architectureStatement
    pure $ vsep $ libHeaderSection subPortsHaveLogic ++ [
      line "architecture \{archName} of \{entityName} is"
    , defaultIndent $ archDecl
    , line "begin"
    , defaultIndent $ archState
    , line "end \{archName};"
    ]

  -- Print entity and architecture (todo: print configuration)
  printCurrentDesign : Gen0 $ Doc opts
  printCurrentDesign = do
    ent <- printEntity
    arch <- printArchitecture
    pure $ vsep
      [
        ent
      , emptyLine
      , arch
    ]

export
prettyDesign : {opts : _} -> {dus : _} -> Fuel ->
               (pds : PrintableDesigns VHDL dus) -> UniqNames dus.length (allDesignNames pds) => VHDLDesign dus -> Gen0 $ Doc opts
prettyDesign _ _         End                                      = pure empty
prettyDesign x pds @{un} (New {s} {usl} {subUs} {mcs} design cont) = do
  let (_ ** keywords) = vhdlKeywordsFor design
  (entityName ** connNames ** subEntNames ** unDes ** unEntConnSub) <- genPDNames keywords x pds {un} design
  let allEntConSubNames : ?
      allEntConSubNames = subEntNames ++ connNames ++ entityName :: allDesignNames pds
  let mcsNames : ?
      mcsNames = toVect connNames
  let subEntNamesVect : ?
      subEntNamesVect = toVect subEntNames

  -- Generate architecture name
  (archName ** unEntConnSubArch) <- genOneName x allEntConSubNames unEntConnSub

  -- Resolve names
  let topNames = resolveInpsOutsNames design mcsNames
  let generatedPrintableInfo : ?
      generatedPrintableInfo = MkPrintableDesign entityName (UserModule topNames)

  cur <- printCurrentDesign x {opts} entityName archName s topNames subEntNamesVect mcs mcsNames (allDesignNames pds) pds design

  -- Print next designs
  recur <- prettyDesign {opts} x (generatedPrintableInfo :: pds) @{unDes} cont
  pure $ vsep
    [
      cur
    , emptyLine
    , recur
  ]
