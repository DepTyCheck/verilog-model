module Runner

import Data.Fuel
import Data.List.Lazy
import Data.List.Lazy.Extra
import Data.List1
import Data.String

import Test.DepTyCheck.Gen
import Test.DepTyCheck.Gen.Coverage

import Test.Verilog
import Test.Verilog.Gen
import Test.Verilog.Pretty
import Test.Verilog.Pretty.Derived

import Text.PrettyPrint.Bernardy

import System
import System.GetOpts
import System.Random.Pure.StdGen
import System.Directory

%default total

StdModules : ModuleSigsList
StdModules =
  [ MkModuleSig [Var Logic', Var Logic'] [Var Logic']
  , MkModuleSig [Var Logic', Var Logic'] [Var Logic']
  , MkModuleSig [Var Logic', Var Logic'] [Var Logic']
  , MkModuleSig [Var Logic', Var Logic'] [Var Logic']
  , MkModuleSig [Var Logic']             [Var Logic']
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
  randomSeed : m StdGen
  layoutOpts : m LayoutOpts
  testsCnt   : m Nat
  modelFuel  : m Fuel
  testsDir   : m (Maybe String)
  covFile    : m (Maybe String)

allNothing : Config Maybe
allNothing = MkConfig
  { randomSeed = Nothing
  , layoutOpts = Nothing
  , testsCnt   = Nothing
  , modelFuel  = Nothing
  , testsDir   = Nothing
  , covFile    = Nothing
  }

defaultConfig : IO $ Config Prelude.id
defaultConfig = pure $ MkConfig
  { randomSeed = !initSeed
  , layoutOpts = Opts 152
  , testsCnt   = 10
  , modelFuel  = limit 4
  , testsDir   = Nothing
  , covFile    = Nothing
  }

-- TODO to do this with `barbies`
mergeCfg : (forall a. m a -> n a -> k a) -> Config m -> Config n -> Config k
mergeCfg f (MkConfig rs lo tc mf td cf) (MkConfig rs' lo' tc' mf' td' cf') = MkConfig (f rs rs') (f lo lo') (f tc tc') (f mf mf') (f td td') (f cf cf')

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
  [ MkOpt [] ["seed"]
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
      "Sets the file path to save the model coverage"
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

countDigit : Nat -> Nat
countDigit 0 = 0
countDigit n = 1 + countDigit(assert_smaller n $ divNatNZ n 10 %search)

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
showSeed gen = let (seed, gamma) = extractRaw gen in "seed_\{show seed}_\{show gamma}"

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

printModule : Maybe String -> Nat -> Nat -> String -> StdGen -> IO ()
printModule testsDir testsCnt idx generatedModule seed = case testsDir of
  Nothing => do
    putStrLn "// Seed: \{showSeed seed}\n-------------------\n"
    putStr $ generatedModule
  Just path => do
    let padding = countDigit testsCnt
    let fileName = "\{path}/\{padLeft padding '0' (show idx)}-\{showSeed seed}.sv"
    writeRes <- writeFile fileName generatedModule
    case writeRes of
      Left err => ignore $ fPutStrLn stderr $ show err
      Right () => putStrLn "[+] Printed file \{fileName}"

printMCov : CoverageGenInfo a -> String -> IO ()
printMCov cgi path = do
  Right () <- writeFile path $ show @{Colourful} cgi | Left err => die "Couldn't write the model coverage to file: \{show err}"
  pure ()

covering
main : IO ()
main = do
  let usage : Lazy String := usageInfo "\nUsage:" cliOpts
  MkResult options [] [] [] <- getOpt Permute cliOpts . tail'' <$> getArgs
    | MkResult {nonOptions=nonOpts@(_::_), _}     => die "unrecognised arguments \{show nonOpts}\{usage}"
    | MkResult {unrecognized=unrecOpts@(_::_), _} => die "unrecodnised options \{show unrecOpts}\{usage}"
    | MkResult {errors=es@(_::_), _}              => die "arguments parse errors \{show es}\{usage}"
  let cfg : Config Maybe = foldl (mergeCfg (\x, y => x <|> y)) allNothing options
  let cfg : Config Prelude.id = mergeCfg (\m, d => fromMaybe d m) cfg !defaultConfig

  let cgi = initCoverageInfo' `{Modules}

  putStrLn "// initial seed: \{show cfg.randomSeed}"
  let vals = unGenTryAllD' cfg.randomSeed $
               genModules cfg.modelFuel StdModules >>= map (render cfg.layoutOpts) . prettyModules (limit 1000) StdModulesPV
  let vals = flip mapMaybe vals $ \gmd => snd gmd >>= \(mcov, md) : (ModelCoverage, String) => if nonTrivial md then Just (fst gmd, mcov, md) else Nothing
  let vals = take (limit cfg.testsCnt) vals

  -- Make sure the paths for the files exist
  whenJust cfg.covFile ensureParentDir
  whenJust cfg.testsDir $ createDir''

  let (seeds, modules) = unzip vals
  let alignedSeeds = cfg.randomSeed::seeds
  let indexedVals = withIndex $ zip alignedSeeds modules

  forS_ cgi indexedVals $ \cgi, (idx, seed, mcov, generatedModule) => do
    printModule cfg.testsDir cfg.testsCnt idx generatedModule seed

    let cgi = registerCoverage mcov cgi
    whenJust cfg.covFile $ printMCov cgi
    pure cgi
