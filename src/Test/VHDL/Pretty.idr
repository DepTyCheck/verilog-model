module Test.VHDL.Pretty

import Data.Fuel
import Data.Vect
import Data.Vect.Extra
import Data.List
import Data.Fin

import Test.Common.Design
import public Test.Common.PrintableDesigns
import public Test.Common.UniqueNames
import Test.VHDL.VHDLDesign
import public Test.VHDL.UniqueNames
import Test.VHDL.VHDLType

import Test.DepTyCheck.Gen
import Text.PrettyPrint.Bernardy

import Debug.Trace

%default total

dtToVt : DataType VHDL -> VHDLType
dtToVt (VHD x) = x

vtypeOf : {s : _} -> {usl : _} -> {subUs : _} -> MultiConnection VHDL s usl subUs -> VHDLType
vtypeOf mc = dtToVt $ typeOf mc

printType : (type : VHDLType) -> Gen0 String
printType (Enum CHARACTER)      = pure $ "character"
printType (Enum BIT)            = pure $ "bit"
printType (Enum BOOLEAN)        = pure $ "boolean"
printType (Enum SEVERITY_LEVEL) = pure $ "severity_level"
printType Integer'              = pure $ "integer"
printType Physical              = pure $ "time"
printType Real                  = pure $ "real"
printType StdLogic              = pure $ "std_logic"

printPort : (name : String) -> (dir : String) -> (type : VHDLType) -> Gen0 String
printPort name dir t = do
  ts <- printType t
  pure $ "\{name} : \{dir} \{ts}"

debugNE : MCNotEmpty a b c d -> String
debugNE JustTSC = "JustTSC"
debugNE JustTSK = "JustTSK"
debugNE JustSSC = "JustSSC"
debugNE JustSSK = "JustSSK"

export
debugMFin : MFin n -> String
debugMFin Nothing  = "Nothing"
debugMFin (Just x) = show $ finToNat x

export
debugFS : (subUs : FinsList n) -> String
debugFS []        = "EndFS"
debugFS (f :: fs) = "\{show $ finToNat f}, " ++ debugFS fs

export
debugMCS : (mcs : MultiConnectionsList l s usl subUs) -> String
debugMCS [] = "End"
debugMCS ((MkMC tsc tsk ssc ssk @{ne}) :: y) = "mcs@{tsc: \{debugMFin tsc}, tsk: \{debugMFin tsk}, ssc: \{debugFS ssc}, ssk: \{debugFS ssk}, ne: \{debugNE ne}}, " ++ debugMCS y

export
debugS : (s : DesignUnitSig l) -> String
debugS s = "s(p len: \{show s.portsCnt} = \{show $ totalTops' s})"

export
debugUsl : (usl : DesignUnitSigsList l) -> String
debugUsl []         = "EndDUS"
debugUsl (d :: usl) = "\{debugS d}, " ++ debugUsl usl

-- export
-- debugGen : {l : Lang} -> {s : DesignUnitSig l} -> {usl : DesignUnitSigsList l} -> {subUs : FinsList usl.length} ->
--            {mcs : MultiConnectionsList l s usl subUs} -> (basic : DesignUnit s usl subUs mcs) -> String
-- debugGen (MkDesign s subUs mcs mc) = d mc where
--   debugFT : {topi : Nat} -> FillTop l s usl subUs [] topi filledTop -> String
--   debugFT {topi = 0} TEnd = "FillTop: end"
--   debugFT {topi = (S k)} (TNew recur {mid} {k} {jf} {topF}) = "FillTop: new top. \{debugS s} k: \{show k} natToFin: \{show $ natToFin k (totalTops' s)} \n" ++ debugFT recur
--   -- debugFT {topi} TEnd = "FillTop: end"
--   -- debugFT {topi} (TNew recur) = ?bhjnk -- "FillTop: new top " ++ show (finToNat topF) ++ "\n" ++ debugFT recur

--   d : (mc : GenMulticonns l s usl subUs mcs) -> String
--   d (GenMC ft fs) = debugFT ft

-- public export
-- topiToTotal' : {s : _} -> Fin (s.inpsCount) -> Fin (totalTops' s)
-- topiToTotal' f = fixDTLFin $ weakenN s.outsCount f

-- public export
-- topoToTotal' : {s : _} -> Fin (s.outsCount) -> Fin (totalTops' s)
-- topoToTotal' f = fixDTLFin $ shift s.inpsCount f

libHeaderOrEmpty : {opts : _} -> Bool -> Doc opts
libHeaderOrEmpty True = vsep [
      line "library ieee;"
    , line "use ieee.std_logic_1164.all;"
    ]
libHeaderOrEmpty False = empty

  -- libHeader : {opts : _} -> Doc opts
  -- libHeader = 

||| Creates the formal => actual association
||| formal: the name inside the sub-component
||| actual: the signal name in the current architecture
assoc : {opts : _} -> String -> String -> Doc opts
assoc formal actual = line "\{formal} => \{actual}"

-- MOVE TO UTILS
toVect : (fs : FinsList n) -> Vect (fs.length) (Fin n)
toVect []        = []
toVect (x :: xs) = x :: toVect xs

Show VHDLPortMode where
  show In      = "in"
  show Out     = "out"
  show InOut   = "inout"
  show Buffer  = "buffer"
  show Linkage = "linkage"


Show (PortMode VHDL) where
  show (VHP x) = show x



-- ||| Find type of port by fin 
-- public export
-- findTypeTop' : {l : _} -> {s : _} -> {usl : _} -> {subUs : _} ->
--               Fin (totalTops' s) -> (mcs : MultiConnectionsList l s usl subUs) -> Maybe $ DataType l
-- findTypeTop' f []                                                              = Nothing
-- findTypeTop' f (mc@(MkMC (Just x) _        _     _      {ne = JustTSC}) :: xs) = trace "\{show $ finToNat f} \{show $ finToNat x} \{show $ f == x}. uslLen: \{show $ usl.length}" $ case f == x of
--   False => findTypeTop' f xs
--   True  => Just $ typeOf mc
-- findTypeTop' f (mc@(MkMC _        (Just x) _     _      {ne = JustTSK}) :: xs) = case f == x of
--   False => findTypeTop' f xs
--   True  => Just $ typeOf mc
-- findTypeTop' f ((   MkMC _        _       (_::_) _      {ne = JustSSC}) :: xs) = findTypeTop' f xs
-- findTypeTop' f ((   MkMC _        _       _      (_::_) {ne = JustSSK}) :: xs) = findTypeTop' f xs


parameters {opts : LayoutOpts} (entityName : String) (archName : String)
           (s : DesignUnitSig VHDL) (topNames : Vect (s.portsCnt) String)
           {usl : _} {subUs : _} (subEntNames : Vect (subUs.length) String)
           (mcs : MultiConnectionsList VHDL s usl subUs) (mcsNames : Vect (length mcs) String)
           (prevEntNames : SVect $ usl.length) (pds : PrintableDesigns VHDL usl)

  -- printInpPort : Fin (s.inpsCount) -> Gen0 $ Doc opts
  -- printInpPort f = case findTypeTI {l=VHDL} f mcs of
  --   Nothing        => pure $ line "(error: printInpPort nothing f: \{show $ finToNat f})" -- impossible
  --   (Just $ VHD t) => do
  --     ps <- printPort (index f topInpNames) "in" t
  --     pure $ line ps

  -- printOutPort : Fin (s.outsCount) -> Gen0 $ Doc opts
  -- printOutPort f = case findTypeTO {l=VHDL} f mcs of
  --   Nothing        => pure $ line "(error: printOutPort nothing f: \{show $ finToNat f})" -- impossible
  --   (Just $ VHD t) => do
  --     ps <- printPort (index f topOutNames) "out" t
  --     pure $ line ps

  printTopPort : Fin (totalTops' s) -> Gen0 $ Doc opts
  printTopPort f = case findTypeTop {l=VHDL} f mcs of
    Nothing        => pure $ line "(error: printTopPort did not found top port type f: \{show $ finToNat f} s.length: \{show s.portsCnt})" -- impossible
    (Just $ VHD t) => do
      ps <- printPort (index f topNames) (show $ topPortMode s f) t
      pure $ line ps


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
    tops <- printIt (s.portsCnt) printTopPort
    pure $ defaultIndent $ vsep [
      generalList (line "port (") (line ");") semi tops
    ]
  
  ||| Is mc is connection to top port? 
  isMCTopPort : Fin (length mcs) -> Bool
  isMCTopPort f with (index mcs f)
    isMCTopPort f | (MkMC (Just _) Nothing _ _ @{_} @{OnlyTSC}) = True
    isMCTopPort f | (MkMC Nothing (Just _) _ _ @{_} @{OnlyTSK}) = True
    isMCTopPort f | (MkMC Nothing Nothing  _ _ @{_} @{NoTop})   = False
  
  portsHaveLogic : List (Fin $ length mcs) -> Bool
  portsHaveLogic fins = foldl (\b, f => if (VHD StdLogic == (typeOf $ index mcs f)) then True else b) False fins

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
    pure $ vsep [
      libHeaderOrEmpty topPortsHaveLogic
    , emptyLine
    , line "entity \{entityName} is"
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

  findSubName : Fin (totalSubs' usl subUs) -> String
  findSubName f = case isSubPortOf f mcs of
    Nothing   => "(findSubName error \{show $ finToNat f})"
    Just mcsF => index mcsF mcsNames

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

      let subNames = map findSubName $ ps
      conns <- connections subNames subFin

      pure $ generalList (line "port map (") (line ");") comma $ conns

  concurrentSubEntDecl : (String, Fin (subUs.length)) -> Gen0 $ Doc opts
  concurrentSubEntDecl (name, subFin) = do
    pm <- portMap subFin
    pure $ vsep [
      subEntityHeader name subFin
    , defaultIndent $ pm
    ]

  ||| prints subUs
  subEntitiesPortsMap : Gen0 $ List $ Doc opts
  subEntitiesPortsMap = traverse concurrentSubEntDecl $ toList $ zip subEntNames $ Vect.allFins $ subUs.length


  ||| 3.3.3 Architecture statement part
  |||
  ||| IEEE 1076-2019
  architectureStatement : Gen0 $ Doc opts
  architectureStatement = do
    entities <- subEntitiesPortsMap
    pure $ vsep [
      vsep entities
    ]

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
    pure $ vsep [
      libHeaderOrEmpty subPortsHaveLogic
    , emptyLine
    , line "architecture \{archName} of \{entityName} is"
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
        -- line (debugGen basic),
        -- line "-- s: \{debugS s} | mcs : \{debugMCS mcs} | subUs : \{debugFS subUs} | usl : \{debugUsl usl}",
        ent
      , emptyLine
      , arch
    ]

export
prettyDesign : {opts : _} -> {dus : _} -> Fuel ->
               (pds : PrintableDesigns VHDL dus) -> UniqNames dus.length (allDesignNames pds) => VHDLDesign dus -> Gen0 $ Doc opts
prettyDesign _ _         End                                      = pure empty
prettyDesign x pds @{un} (New {s} {usl} {subUs} {mcs} basic cont) = do
  (entityName ** connNames ** subEntNames ** unDes ** unEntConnSub) <- genPDNames VHDLKeywords x pds {un} basic
  let allEntConSubNames : ?
      allEntConSubNames = subEntNames ++ connNames ++ entityName :: allDesignNames pds
  let mcsNames : ?
      mcsNames = toVect connNames
  let subEntNamesVect : ?
      subEntNamesVect = toVect subEntNames

  -- Generate architecture name
  (archName ** unEntConnSubArch) <- genOneName x allEntConSubNames unEntConnSub

  -- Resolve names
  let (topNames) = resolveInpsOutsNames basic mcsNames
  let generatedPrintableInfo : ?
      generatedPrintableInfo = MkPrintableDesign entityName (UserModule topNames)

  -- Recursive call to use at the end
  recur <- prettyDesign {opts} x (generatedPrintableInfo :: pds) @{unDes} cont

  -- Finally print it
  cur <- printCurrentDesign {opts} entityName archName s topNames subEntNamesVect mcs mcsNames (allDesignNames pds) pds
  pure $ vsep
    [
      cur
    , emptyLine
    , recur
  ]
