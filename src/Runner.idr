module Runner

import Data.Fuel
import Data.List
import Data.List.Lazy
import Data.List.Lazy.Extra
import Data.List1
import Data.String
import Data.Fin
import Data.Vect.Extra
import Debug.Trace

import Test.DepTyCheck.Gen
import Test.DepTyCheck.Gen.Coverage

import Test.Verilog.Connections.Derived
import Test.Verilog.Assign.Derived
import Test.Verilog.Literal.Derived

import Test.Verilog.Pretty

import Data.Fin.ToFin

import Text.PrettyPrint.Bernardy

import System
import System.GetOpts
import System.Random.Pure.StdGen
import System.Directory

%default total

StdModules : ModuleSigsList
StdModules =
  [ MkModuleSig [Var $ SVar Logic', Var $ SVar Logic'] [Var $ SVar Logic']
  , MkModuleSig [Var $ SVar Logic', Var $ SVar Logic'] [Var $ SVar Logic']
  , MkModuleSig [Var $ SVar Logic', Var $ SVar Logic'] [Var $ SVar Logic']
  , MkModuleSig [Var $ SVar Logic', Var $ SVar Logic'] [Var $ SVar Logic']
  , MkModuleSig [Var $ SVar Logic']                    [Var $ SVar Logic']
  ]

StdModulesPV : PrintableModules StdModules
StdModulesPV =
  [
    MkPrintableModule "and"  (StdModule 2 1)
  , MkPrintableModule "or"   (StdModule 2 1)
  , MkPrintableModule "nand" (StdModule 2 1)
  , MkPrintableModule "xor"  (StdModule 2 1)
  , MkPrintableModule "not"  (StdModule 1 1)
  ]

record Config m where
  constructor MkConfig
  help       : m Bool
  randomSeed : m StdGen
  layoutOpts : m LayoutOpts
  testsCnt   : m Nat
  modelFuel  : m Fuel
  testsDir   : m (Maybe String)
  covFile    : m (Maybe String)
  seedInName : m Bool
  seedInFile : m Bool
  silent     : m Bool

allNothing : Config Maybe
allNothing = MkConfig
  { help       = Nothing
  , randomSeed = Nothing
  , layoutOpts = Nothing
  , testsCnt   = Nothing
  , modelFuel  = Nothing
  , testsDir   = Nothing
  , covFile    = Nothing
  , seedInName = Nothing
  , seedInFile = Nothing
  , silent     = Nothing
  }

Cfg : Type
Cfg = Config Prelude.id

defaultConfig : IO $ Cfg
defaultConfig = pure $ MkConfig
  { help       = False
  , randomSeed = !initSeed
  , layoutOpts = Opts 152
  , testsCnt   = 10
  , modelFuel  = limit 4
  , testsDir   = Nothing
  , covFile    = Nothing
  , seedInName = False
  , seedInFile = False
  , silent     = False
  }

-- TODO to do this with `barbies`
mergeCfg : (forall a. m a -> n a -> k a) -> Config m -> Config n -> Config k
mergeCfg f (MkConfig h rs lo tc mf td cf sn sf s) (MkConfig h' rs' lo' tc' mf' td' cf' sn' sf' s') =
 MkConfig  (f h h') (f rs rs') (f lo lo') (f tc tc') (f mf mf') (f td td') (f cf cf') (f sn sn') (f sf sf') (f s s')

parseSeed : String -> Either String $ Config Maybe
parseSeed str = do
  let n1:::n2::[] = trim <$> split (== ',') str
    | _ => Left "we expect two numbers divided by a comma"
  let Just n1 = parsePositive n1
    | Nothing => Left "expected a positive number at the first component, given `\{n1}`"
  let Just n2 = parsePositive {a=Bits64} n2
    | Nothing => Left "expected a positive number at the second component, given `\{n2}`"
  let Yes prf = decSo $ testBit n2 0
    | No _ => Left "second component must be odd"
  Right $ {randomSeed := Just $ rawStdGen n1 n2} allNothing

parsePPWidth : String -> Either String $ Config Maybe
parsePPWidth str = case parsePositive str of
  Just n  => Right $ {layoutOpts := Just $ Opts n} allNothing
  Nothing => Left "can't parse max width for the pretty-printer"

parseTestsCount : String -> Either String $ Config Maybe
parseTestsCount str = case parsePositive str of
  Just n  => Right $ {testsCnt := Just n} allNothing
  Nothing => Left "can't parse given count of tests"

parseModelFuel : String -> Either String $ Config Maybe
parseModelFuel str = case parsePositive str of
  Just n  => Right $ {modelFuel := Just $ limit n} allNothing
  Nothing => Left "can't parse given model fuel"

parseTestsDir : String -> Either String $ Config Maybe
parseTestsDir str = Right $ {testsDir := Just $ Just str} allNothing

parseCovFile : String -> Either String $ Config Maybe
parseCovFile str = Right $ {covFile := Just $ Just str} allNothing

cliOpts : List $ OptDescr $ Config Maybe
cliOpts =
  [ MkOpt ['h'] ["help"]
      (NoArg $ {help := Just $ True} allNothing)
      "Display the help message and exit."
  , MkOpt [] ["seed"]
      (ReqArg' parseSeed "<seed>,<gamma>")
      "Sets particular random seed to start with."
  , MkOpt ['w'] ["pp-width"]
      (ReqArg' parsePPWidth "<nat>")
      "Sets the max length for the pretty-printer."
  , MkOpt ['n'] ["tests-count"]
      (ReqArg' parseTestsCount "<tests-count>")
      "Sets the count of tests to generate."
  , MkOpt [] ["model-fuel"]
      (ReqArg' parseModelFuel "<fuel>")
      "Sets how much fuel there is for generation of the model."
  , MkOpt ['o'] ["to", "generate-to"]
      (ReqArg' parseTestsDir "<target-dir>")
      "Sets where to generate the tests."
  , MkOpt [] ["coverage"]
      (ReqArg' parseCovFile "<coverage>")
      "Sets the file path to save the model coverage."
  , MkOpt [] ["seed-name"]
      (NoArg $ {seedInName := Just $ True} allNothing)
      "Adds a seed to the names of generated files."
  , MkOpt [] ["seed-content"]
      (NoArg $ {seedInFile := Just $ True} allNothing)
      "Prints the initial-seed in the first line and the seed-after in the last line of the test file."
  , MkOpt [] ["silent"]
      (NoArg $ {silent := Just $ True} allNothing)
      "Disables all output to the console and files, useful for benchmarking."
  ]

tail'' : List a -> List a
tail'' []        = []
tail'' xs@(_::_) = tail xs

covering
mapMaybe : (a -> Maybe b) -> Stream a -> Stream b
mapMaybe f (x::xs) = case f x of
  Just y  => y :: mapMaybe f xs
  Nothing => mapMaybe f xs

nonTrivial : String -> Bool
nonTrivial = any (/= "") . map trim . lines

createDir' : String -> IO (Either FileError ())
createDir' p = foldlM createDirHelper (Right ()) $ inits $ toList $ split (=='/') p where
  pr = if isPrefixOf "/" p then "/" else ""
  createDirHelper : Either FileError () -> List String -> IO (Either FileError ())
  createDirHelper _            []        = pure $ Right ()
  createDirHelper (Left  err)  _         = pure $ Left err
  createDirHelper (Right _  )  subpaths  = createDir (pr ++ joinBy "/" subpaths) <&> \case
    Left FileExists => Right ()
    e               => e

showSeed: StdGen -> String
showSeed gen = let (seed, gamma) = extractRaw gen in "\{show seed},\{show gamma}"

forS_ : Monad f => (seed : s) -> LazyList a -> (s -> a -> f s) -> f ()
forS_ seed []      g = pure ()
forS_ seed (x::xs) g = forS_ !(g seed x) xs g

-- A shortcut for createDir'
createDir'' : String -> IO ()
createDir'' path = do
  Right () <- createDir' path | Left err => die "Couldn't create dirs for path '\{path}' due to an error: \{show err}"
  pure()

-- Creates dirs for the file path
ensureParentDir : String -> IO ()
ensureParentDir path = case init $ split (=='/') path of
  []   => pure ()
  dirs => createDir'' $ joinBy "/" dirs

content : Cfg -> String -> StdGen -> StdGen -> String
content cfg generatedModule initialSeed seedAfter = case cfg.seedInFile of
  False => generatedModule
  True  => "// Seed: \{showSeed initialSeed}\n\n"
        ++ generatedModule
        ++ "\n// Seed after: \{showSeed seedAfter}\n"

fileName : Cfg -> String -> Nat -> StdGen -> String
fileName cfg path idx initialSeed = do
  let seedSuffix = if cfg.seedInName then "-seed_\{showSeed initialSeed}" else ""
  "\{path}/\{show $ S idx}\{seedSuffix}.sv"

printModule : Cfg -> Nat -> String -> StdGen -> StdGen -> IO ()
printModule cfg idx generatedModule initialSeed seedAfter = do
  let text = content cfg generatedModule initialSeed seedAfter
  case cfg.testsDir of
    Nothing   => putStr $ text ++ "--------------------------\n\n"
    Just path => do
      let file = fileName cfg path idx initialSeed
      writeRes <- writeFile file text
      case writeRes of
        Left err => ignore $ fPutStrLn stderr $ show err
        Right () => putStrLn "[+] Printed file \{file}"

printMCov : CoverageGenInfo a -> String -> IO ()
printMCov cgi path = do
  Right () <- writeFile path $ show @{Colourful} cgi | Left err => die "Couldn't write the model coverage to file: \{show err}"
  pure ()

finLookup : (y : FinsList n) -> (List $ Fin $ y.length) -> List $ Fin n
finLookup xs []        = []
finLookup xs (y :: ys) = index xs y :: finLookup xs ys

selectPorts' : (mcs : MultiConnectionsList ms m subMs) -> List (Fin $ length mcs) -> SVObjList
selectPorts' p []        = []
selectPorts' p (x :: xs) = (typeOf $ index p x) :: selectPorts' p xs

export
toFinsList : {mcs : MultiConnectionsList m ms subMs} -> MDAssigns mcs -> FinsList (length mcs)
toFinsList []      = []
toFinsList (x::xs) = x :: toFinsList xs

-- tryToFitL : {to : _} -> List (Fin a) -> List (Fin to)
-- tryToFitL []      = []
-- tryToFitL (x::xs) = case tryToFit x of
--   Nothing => tryToFitL xs
--   Just x' => x' :: tryToFitL xs

-- debugd1 : PortRef [] [] [] []
-- debugd1 = Sub $ ACCNil

-- DebugMs : FinsList $ ModuleSig.length StdModules
-- DebugMs = [FZ]

-- debugM : ModuleSig
-- debugM = MkModuleSig [Var $ SVar Logic'] [Var $ SVar Logic']

-- DebugLTO : FinsList $ Runner.debugM.outsCount
-- DebugLTO = [FZ]

-- genFin' : (n : Nat) -> Gen MaybeEmpty $ Fin n
-- genFin' Z     = empty
-- genFin' (S n) = elements (fromList (allFins (S n)))

-- public export
-- toList : MultiConnectionsList ms m subMs -> List $ MultiConnection ms m subMs
-- toList []      = []
-- toList (x::xs) = x :: toList xs

-- canConnect' : SVType -> SVType -> Bool
-- canConnect' a b = if (isUnpacked' a) || (isUnpacked' b) 
--   then (bitsCnt a == bitsCnt b) && (states a == states b) && (isSigned a == isSigned b) && (basicIntegral a == basicIntegral b) 
--   else True

-- canConnect : SVObject -> SVObject -> Bool
-- canConnect a b = canConnect' (valueOf a) (valueOf b)

-- public export
-- Eq (MFin n) where
--   (==) Nothing  Nothing  = True
--   (==) Nothing  (Just x) = False
--   (==) (Just x) Nothing  = False
--   (==) (Just x) (Just y) = x == y

-- public export
-- Eq (FinsList n) where
--   (==) Nil       Nil       = True
--   (==) Nil       (_ :: _)  = False
--   (==) (_ :: _)  Nil       = False
--   (==) (x :: xs) (y :: ys) = x == y && xs == ys

-- Eq (MultiConnection ms m subMs) where
--   (==) (MkMC tsk ssk tsc ssc) (MkMC tsk' ssk' tsc' ssc') = tsk == tsk' && ssk == ssk' && tsc == tsc' && ssc == ssc'

-- elem : FinsList n -> Fin n -> Bool
-- elem []      f = False
-- elem (x::xs) f = if x == f then True else elem xs f

-- public export
-- replaceOn : (fs : MultiConnectionsList ms m subMs) -> (old : MultiConnection ms m subMs) -> 
--             (new : MultiConnection ms m subMs) -> MultiConnectionsList ms m subMs
-- replaceOn []        _   _ = []
-- replaceOn (x :: xs) old new = if x == old 
--   then new :: xs
--   else x :: replaceOn xs old new

-- parameters (ms : ModuleSigsList) (m : ModuleSig) (subMs : FinsList ms.length) (ports : SVObjList) 
--           --  (newAny : Fin (ports.length) -> MultiConnection ms m subMs)
--            (superReplace : Fin (ports.length) -> MultiConnection ms m subMs -> Maybe $ MultiConnection ms m subMs)
--           --  (isOkay : Fin (ports.length) -> MultiConnection ms m subMs -> Bool)
  
--   filterAny : Fin (ports.length) -> MultiConnectionsList ms m subMs -> MultiConnectionsList ms m subMs
--   filterAny f []      = []
--   filterAny f (x::xs) = case superReplace f x of
--     (Just _) => x :: filterAny f xs
--     Nothing  => filterAny f xs

--   filterCanConnect : SVObject -> MultiConnectionsList ms m subMs -> MultiConnectionsList ms m subMs
--   filterCanConnect t []      = []
--   filterCanConnect t (x::xs) = if canConnect (typeOf x) t then x :: filterCanConnect t xs else filterCanConnect t xs

--   extistingAny : Fuel -> Fin (ports.length) -> MultiConnectionsList ms m subMs -> Gen MaybeEmpty $ MultiConnectionsList ms m subMs
--   extistingAny x f []          = pure $ [ newAny f ]
--   extistingAny x f rest@(_::_) = do
--     let filtered = filterCanConnect (typeOf ports f) $ filterAny f rest
--     case filtered of
--       []                => pure $ newAny f :: rest
--       filtered'@(x::xs) => do
--         oldEl <- elements $ fromList $ toList filtered'
--        superReplace f oldEl
--         pure $ replaceOn rest oldEl newEl

--   fillAny : Fuel -> FinsList (ports.length) -> MultiConnectionsList ms m subMs -> Gen MaybeEmpty $ MultiConnectionsList ms m subMs
--   fillAny x []      rest = pure rest
--   fillAny x (f::fs) rest = do
--     cur <- fillAny x fs rest
--     pure $ newAny f :: cur
--     -- oneOf [pure $ newAny f :: cur, extistingAny x f cur]
    

-- isOkayTK : {ms : ModuleSigsList} -> {m : ModuleSig} -> {subMs : FinsList ms.length} -> 
--            Fin (topSnks' m) -> MultiConnection ms m subMs -> Bool
-- isOkayTK _ (MkMC Nothing  ssk Nothing ssc) = True
-- isOkayTK _ _                               = False

-- newTK : {ms : ModuleSigsList} -> {m : ModuleSig} -> {subMs : FinsList ms.length} -> 
--         Fin (topSnks' m) -> MultiConnection ms m subMs
-- newTK f = MkMC (Just f) [] Nothing []
 
-- replaceTK : Fin (topSnks' m) -> MultiConnection ms m subMs -> Gen MaybeEmpty $ MultiConnection ms m subMs
-- replaceTK f (MkMC Nothing ssc Nothing ssk) = pure $ MkMC (Just f) ssc Nothing ssk
-- replaceTK f _                              = empty

-- superReplace : Fin (topSnks' m) -> MultiConnection ms m subMs -> Maybe $ MultiConnection ms m subMs

-- isOkaySK : {ms : ModuleSigsList} -> {m : ModuleSig} -> {subMs : FinsList ms.length} -> 
--            Fin (subSnks' ms m subMs) -> MultiConnection ms m subMs -> Bool
-- isOkaySK f (MkMC tsk ssk tsc ssc) = elem ssk f

-- newSK : {ms : ModuleSigsList} -> {m : ModuleSig} -> {subMs : FinsList ms.length} -> 
--         Fin (subSnks' ms m subMs) -> MultiConnection ms m subMs
-- newSK f = MkMC Nothing [ f ] Nothing []

-- replaceSK : Fin (subSnks' ms m subMs) -> MultiConnection ms m subMs -> Gen MaybeEmpty $ MultiConnection ms m subMs
-- replaceSK f (MkMC tsk ssk tsc ssc) = pure $ MkMC tsk (f::ssk) tsc ssc

-- isOkayTC : {ms : ModuleSigsList} -> {m : ModuleSig} -> {subMs : FinsList ms.length} -> 
--            Fin (topSrcs' m) -> MultiConnection ms m subMs -> Bool
-- isOkayTC _ (MkMC Nothing ssk Nothing ssc) = True
-- isOkayTC _ _                              = False

-- newTC : {ms : ModuleSigsList} -> {m : ModuleSig} -> {subMs : FinsList ms.length} -> 
--         Fin (topSrcs' m) -> MultiConnection ms m subMs
-- newTC f = MkMC Nothing [] (Just f) []
 
-- replaceTC : Fin (topSrcs' m) -> MultiConnection ms m subMs -> Gen MaybeEmpty $ MultiConnection ms m subMs
-- replaceTC f (MkMC Nothing ssk Nothing ssc) = pure $ MkMC Nothing ssk (Just f) ssc
-- replaceTC f _                              = empty

-- isOkaySC : {ms : ModuleSigsList} -> {m : ModuleSig} -> {subMs : FinsList ms.length} -> 
--            Fin (subSrcs' ms m subMs) -> MultiConnection ms m subMs -> Bool
-- isOkaySC f (MkMC tsk ssk tsc ssc) = elem ssc f

-- newSC : {ms : ModuleSigsList} -> {m : ModuleSig} -> {subMs : FinsList ms.length} -> 
--         Fin (subSrcs' ms m subMs) -> MultiConnection ms m subMs
-- newSC f = MkMC Nothing [] Nothing [ f ]

-- replaceSC : Fin (subSrcs' ms m subMs) -> MultiConnection ms m subMs -> Gen MaybeEmpty $ MultiConnection ms m subMs
-- replaceSC f (MkMC tsk ssk tsc ssc) = pure $ MkMC tsk ssk tsc (f::ssc)

-- d : MultiConnectionsList ms m subMs -> String
-- d mcs = joinBy "\n" $ "--------------------" :: debugPrint mcs
-- -- 3 weeks wasted trying to make this generator derived
-- genMCL' : Fuel -> (ms : ModuleSigsList) -> (m : ModuleSig) -> (subMs : FinsList ms.length) -> 
--           Gen MaybeEmpty $ MultiConnectionsList ms m subMs
-- genMCL' x ms m subMs = do
--   fillTK <- fillAny ms m subMs (topSnks m)          newTK replaceTK isOkayTK x (allFins $ topSnks' m)          []
--   fillSK <- fillAny ms m subMs (subSnks ms m subMs) newSK replaceSK isOkaySK x (allFins $ subSnks' ms m subMs) fillTK
--   fillTC <- fillAny ms m subMs (topSrcs m)          newTC replaceTC isOkayTC x (allFins $ topSrcs' m)          fillSK
--   fillSC <- fillAny ms m subMs (subSrcs ms m subMs) newSC replaceSC isOkaySC x (allFins $ subSrcs' ms m subMs) fillTC
--   pure $ flip trace fillSC $ joinBy "\n" $ "===============" :: map d [fillTK, fillSK, fillTC, fillSC]

-- export
-- genMCL : Fuel -> (ms' : ModuleSigsList) -> (m' : ModuleSig) -> (subMs' : FinsList ms'.length) -> Gen MaybeEmpty $ MultiConnectionsList ms' m' subMs'
-- genMCL x ms m subMs = withCoverage $ genMCL' x ms m subMs
-- d3 : PortRef [Var $ SVar Logic'] DebugLTO [] []
-- d3 = Top FZ
-- -- d1 = Sub $ ACCOne FZ

-- d2 : Gen MaybeEmpty (n : Nat ** MultiConnectionVect n StdModules (Runner.debugM) DebugMs (UF DebugLTO [] [] []))
-- d2 = pure $ (1 ** Cons d3 (Sub ACCNil) Empty) -- (Sub ACCNil) -- {tsk=FinsList (topSnks' (Runner.debugM))}

gen : Fuel -> Gen MaybeEmpty $ ExtendedModules StdModules
gen x = do
  -- debug <- genPf x [] [] [Var $ SVar Logic', Var $ SVar Logic'] [FZ, FS FZ]
  -- let ms = StdModules
  -- let subMs = 
  -- debug <- genMCL x StdModules debugM DebugMs $ UF DebugLTO [] [] [] --$ UseFins.fullUF {ms=StdModules}
  -- pure End
  rawMS <- genModules x StdModules
  res <- extend x rawMS
  pure res where
    extend : Fuel -> {ms: _} -> Modules ms -> Gen MaybeEmpty $ ExtendedModules ms
    extend _ End = pure End
    extend x modules@(NewCompositeModule m {ms} subMs {mcs} _ cont) = do
      -- Gen Assigns
      -- let sdmcs = portsToAssign mcs
      -- (rawSdAssigns ** ufsd) <- genUniqueFins x (sdmcs.length)
      -- let sdAssigns = finLookup sdmcs rawSdAssigns.asList
      -- rawMdAssigns <- genMDAssigns x mcs
      -- let mdAssigns = (toFinsList rawMdAssigns).asList

      -- -- Gen literals
      -- sdLiterals <- genLiterals @{genBinVect} x $ selectPorts' mcs sdAssigns
      -- mdLiterals <- genLiterals @{genBinVect} x $ selectPorts' mcs mdAssigns

      -- Extend the rest
      contEx <- extend x cont
  
      pure $ NewCompositeModule m subMs mcs [] [] [] [] contEx

-- covering
-- main : IO ()
-- main = do
--   putStrLn "Hello from Idris2222!"

-- public export
-- showFL : FinsList n -> String
-- showFL [] = "Nil"
-- showFL (x :: xs) = "\{show $ finToNat x}, \{showFL xs}"

-- public export
-- showUF : UseFins ms m subMs -> String
-- showUF (UF tsk ssk tsc ssc) = "UF{ \{showFL tsk} ; \{showFL ssk} ; \{showFL tsc} ; \{showFL ssc} }"

-- test : UseFins Runner.StdModules (MkModuleSig [Var $ SVar Logic', Var $ SVar Logic'] [Var $ SVar Logic']) ([FZ, FS FZ])
-- test = minus (UF [FZ] [FS FZ, FZ] [FZ] [FZ]) (UF [FZ] [FZ] [] [FZ])

covering
main : IO ()
main = do
  putStrLn "msg 1"
  let usage : Lazy String := usageInfo "Usage:" cliOpts
  MkResult options [] [] [] <- getOpt Permute cliOpts . tail'' <$> getArgs
    | MkResult {nonOptions=nonOpts@(_::_), _}     => die "unrecognised arguments \{show nonOpts}\n\{usage}"
    | MkResult {unrecognized=unrecOpts@(_::_), _} => die "unrecognised options \{show unrecOpts}\n\{usage}"
    | MkResult {errors=es@(_::_), _}              => die "arguments parse errors \{show es}\n\{usage}"
  let cfg : Config Maybe = foldl (mergeCfg (\x, y => x <|> y)) allNothing options
  let cfg : Cfg = mergeCfg (\m, d => fromMaybe d m) cfg !defaultConfig

  when cfg.help $ do
    putStrLn usage
    exitSuccess
  
  putStrLn "msg 2"
  let cgi = initCoverageInfo'' [`{Modules}, `{LiteralsList},`{MDAssigns}, `{SDAssigns} ]
  putStrLn "msg 3"
  let vals = unGenTryAllD' cfg.randomSeed $ gen cfg.modelFuel >>= map (render cfg.layoutOpts) . prettyModules (limit 1000) StdModulesPV
  putStrLn "msg 4"
  let vals = flip mapMaybe vals $ \gmd => snd gmd >>= \(mcov, md) : (ModelCoverage, String) =>
                                                        if nonTrivial md then Just (fst gmd, mcov, md) else Nothing
  putStrLn "msg 5"
  let vals = take (limit cfg.testsCnt) vals
  putStrLn "msg 6"

  -- Make sure the paths for the files exist
  whenJust cfg.covFile ensureParentDir
  whenJust cfg.testsDir $ createDir''

  -- putStrLn $ showUF $ test
  putStrLn "msg 7"
  let (seeds, modules) = unzip vals
  putStrLn "msg 8"
  let alignedSeeds = cfg.randomSeed::seeds
  let indexedVals = withIndex $ zip3 alignedSeeds seeds modules

  forS_ cgi indexedVals $ \cgi, (idx, initialSeed, seedAfter, mcov, generatedModule) => do
    when (not cfg.silent) $ printModule cfg idx generatedModule initialSeed seedAfter

    let cgi = registerCoverage mcov cgi
    whenJust cfg.covFile $ printMCov cgi
    pure cgi
