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

%default total

dtToVt : DataType VHDL -> VHDLType
dtToVt (VHT x) = x

vtypeOf : {s : _} -> {usl : _} -> {subUs : _} -> MultiConnection VHDL s usl subUs -> VHDLType
vtypeOf mc = dtToVt $ typeOf mc

printType : (type : VHDLType) -> Gen0 String
printType StdLogic' = pure $ "std_logic"

printPort : (name : String) -> (dir : String) -> (type : VHDLType) -> Gen0 String
printPort name dir t = do
  ts <- printType t
  pure $ "\{name} : \{dir} \{ts}"

export
debugMCS : (mcs : MultiConnectionsList l s usl subUs) -> String
debugMCS [] = "End"
debugMCS ((MkMC Nothing subs) :: y) = "No, " ++ debugMCS y
debugMCS ((MkMC (Just x) subs) :: y) = "\{show $ finToNat x}, " ++ debugMCS y

export
debugSubUs : (subUs : FinsList n) -> String
debugSubUs []        = "EndFS"
debugSubUs (f :: fs) = "\{show $ finToNat f}, " ++ debugSubUs fs

export
debugS : (s : DesignUnitSig l) -> String
debugS s = "s(i: \{show $ s.inpsCount} + o: \{show s.outsCount} = \{show $ totalTops' s})"

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
-- findTypeTOp' : {l : _} -> {s : _} -> {usl : _} -> {subUs : _} ->
--                Fin (totalTops' s) -> (mcs : MultiConnectionsList l s usl subUs) -> Maybe $ DataType l
-- findTypeTOp' f []        = Nothing -- impossible
-- findTypeTOp' f (   (MkMC Nothing  subs) :: xs) = findTypeTOp' f xs
-- findTypeTOp' f (mc@(MkMC (Just x) subs) :: xs) = case f == x of
--   False => findTypeTOp' f xs
--   True  => Just $ typeOf mc

-- public export
-- topiToTotal' : {s : _} -> Fin (s.inpsCount) -> Fin (totalTops' s)
-- topiToTotal' f = fixDTLFin $ weakenN s.outsCount f

-- public export
-- topoToTotal' : {s : _} -> Fin (s.outsCount) -> Fin (totalTops' s)
-- topoToTotal' f = fixDTLFin $ shift s.inpsCount f

-- public export
-- findTypeTI' : {l : _} -> {s : _} -> {usl : _} -> {subUs : _} ->
--               Fin (s.inpsCount) -> (mcs : MultiConnectionsList l s usl subUs) -> Maybe $ DataType l
-- findTypeTI' f = findTypeTOp' $ topiToTotal' f

-- public export
-- findTypeTO' : {l : _} -> {s : _} -> {usl : _} -> {subUs : _} ->
--               Fin (s.outsCount) -> (mcs : MultiConnectionsList l s usl subUs) -> Maybe $ DataType l
-- findTypeTO' f = findTypeTOp' $ topoToTotal' f

libHeader : {opts : _} -> Doc opts
libHeader = vsep [
    line "library ieee;"
  , line "use ieee.std_logic_1164.all;"
  ]

||| Creates the formal => actual association
||| formal: the name inside the sub-component
||| actual: the signal name in the current architecture
assoc : {opts : _} -> String -> String -> Doc opts
assoc formal actual = line "\{formal} => \{actual}"

-- MOVE TO UTILS
toVect : (fs : FinsList n) -> Vect (fs.length) (Fin n)
toVect []        = []
toVect (x :: xs) = x :: toVect xs

parameters {opts : LayoutOpts} (entityName : String) (archName : String)
           (s : DesignUnitSig VHDL) (topInpNames : Vect (s.inpsCount) String) (topOutNames : Vect (s.outsCount) String)
           {usl : _} {subUs : _} (subEntNames : Vect (subUs.length) String)
           (mcs : MultiConnectionsList VHDL s usl subUs) (mcsNames : Vect (length mcs) String)
           (prevEntNames : SVect $ usl.length) (pds : PrintableDesigns VHDL usl)
           
           

  printInpPort : Fin (s.inpsCount) -> Gen0 $ Doc opts
  printInpPort f = case findTypeTI {l=VHDL} f mcs of
    Nothing        => pure $ line "(error: printInpPort nothing f: \{show $ finToNat f})" -- impossible
    (Just $ VHT t) => do
      ps <- printPort (index f topInpNames) "in" t
      pure $ line ps

  printOutPort : Fin (s.outsCount) -> Gen0 $ Doc opts
  printOutPort f = case findTypeTO {l=VHDL} f mcs of
    Nothing        => pure $ line "(error: printOutPort nothing f: \{show $ finToNat f})" -- impossible
    (Just $ VHT t) => do
      ps <- printPort (index f topOutNames) "out" t
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
  printPortCaluse = do
    inps <- printIt (s.inpsCount) printInpPort
    outs <- printIt (s.outsCount) printOutPort
    pure $ defaultIndent $ vsep [ 
     generalList (line "port (") (line ");") semi $ inps ++ outs
    ]
    -- generalList lparen rparen comma


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
      libHeader
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
  signalDeclaration : (String, MultiConnection VHDL s usl subUs) -> Gen0 $ Doc opts
  signalDeclaration (name, mc) = do
    ts <- printType $ vtypeOf mc
    pure $ line "signal \{name} : \{ts};"
  
  ||| 3.3.2 Architecture declarative part
  |||
  ||| IEEE 1076-2019
  printArchitectureDeclarative : Gen0 $ Doc opts
  printArchitectureDeclarative = do
    signals <- traverse signalDeclaration $ toList $ zip mcsNames $ toVect mcs
    pure $ vsep [
      vsep signals
    ]

  ||| ex: e_b : entity module_b
  subEntityHeader : String -> Fin (length usl) -> Doc opts
  subEntityHeader name uslFin = line "\{name} : entity work.\{index uslFin $ toVect prevEntNames}"

  findSubName : Fin (totalSubs' usl subUs) -> String
  findSubName f = case isSubPortOf f mcs of
    Nothing   => "(findSubName error \{show $ finToNat f})"
    Just mcsF => index mcsF mcsNames
  
  connections : (subNames : List String) -> Fin (subUs.length) -> Gen0 $ List $ Doc opts
  connections subNames subFin  = case (index pds $ index subUs subFin).insOuts of
    StdModule  _      _      => pure $ map line $ toList subNames
    UserModule exInps exOuts => do
      let exNames = toList $ exInps ++ exOuts
      pure $ zipWith assoc exNames subNames

  ||| ex:
  ||| port map (
  |||   a => sig_a,
  |||   b => sig_b,
  |||   y => sig_y
  ||| );
  portMap : Fin (subUs.length) -> Fin (length usl) -> Gen0 $ Doc opts
  portMap subFin uslFin = do
    let inputs  = List.allFins (index usl $ index subUs subFin).inpsCount <&> toTotalSubsInpIdx subFin
    let outputs = List.allFins (index usl $ index subUs subFin).outsCount <&> toTotalSubsOutIdx subFin

    let subNames = map findSubName $ inputs ++ outputs
    conns <- connections subNames subFin

    pure $ generalList (line "port map (") (line ");") comma $ conns

  concurrentSubEntDecl : (String, Fin (subUs.length), Fin (length usl)) -> Gen0 $ Doc opts
  concurrentSubEntDecl (name, subFin, uslFin) = do
    pm <- portMap subFin uslFin
    pure $ vsep [
      subEntityHeader name uslFin
    , defaultIndent $ pm
    ]
  
  ||| prints subUs
  subEntitiesPortsMap : Gen0 $ List $ Doc opts
  subEntitiesPortsMap = traverse concurrentSubEntDecl $ toList $ zip subEntNames $ withIndex $ toVect subUs -- todo remove uslFin


  ||| 3.3.3 Architecture statement part
  |||
  ||| IEEE 1076-2019
  printArchitectureStatement : Gen0 $ Doc opts
  printArchitectureStatement = do
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
    archDecl <- printArchitectureDeclarative
    archState <- printArchitectureStatement
    pure $ vsep [
      libHeader
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
        -- line (debugGen basic), line "-- s: \{debugS s} | mcs : \{debugMCS mcs} | subUs : \{debugSubUs subUs} | usl : \{debugUsl usl}"
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
  let (inpNames, outNames) = resolveInpsOutsNames basic mcsNames
  let generatedPrintableInfo : ?
      generatedPrintableInfo = MkPrintableDesign entityName (UserModule inpNames outNames)

  -- Recursive call to use at the end
  recur <- prettyDesign {opts} x (generatedPrintableInfo :: pds) @{unDes} cont

  -- Finally print it
  cur <- printCurrentDesign {opts} entityName archName s inpNames outNames subEntNamesVect mcs mcsNames (allDesignNames pds) pds
  pure $ vsep
    [
      cur
    , emptyLine
    , recur
  ]
