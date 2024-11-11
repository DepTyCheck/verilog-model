module Runner

import Data.Fuel
import Data.List.Lazy
import Data.List.Lazy.Extra
import Data.List1
import Data.String

import Test.DepTyCheck.Gen

import Test.Verilog
import Test.Verilog.Gen
import Test.Verilog.Pretty
import Test.Verilog.Pretty.Derived

import Text.PrettyPrint.Bernardy

import System
import System.GetOpts
import System.Random.Pure.StdGen
import System.File
import System.Directory

%default total

StdModules : ModuleSigsList
StdModules =
  [ MkModuleSig 2 1
  , MkModuleSig 2 1
  , MkModuleSig 2 1
  , MkModuleSig 2 1
  , MkModuleSig 1 1
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

allNothing : Config Maybe
allNothing = MkConfig
  { randomSeed = Nothing
  , layoutOpts = Nothing
  , testsCnt   = Nothing
  , modelFuel  = Nothing
  , testsDir   = Nothing
  }

defaultConfig : IO $ Config Prelude.id
defaultConfig = pure $ MkConfig
  { randomSeed = !initSeed
  , layoutOpts = Opts 152
  , testsCnt   = 10
  , modelFuel  = limit 4
  , testsDir   = Nothing
  }

-- TODO to do this with `barbies`
mergeCfg : (forall a. m a -> n a -> k a) -> Config m -> Config n -> Config k
mergeCfg f (MkConfig rs lo tc mf td) (MkConfig rs' lo' tc' mf' td') = MkConfig (f rs rs') (f lo lo') (f tc tc') (f mf mf') (f td td')

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
countDigit 0 = 1
countDigit n = 1 + countDigit(assert_smaller n $ divNatNZ n 10 %search)

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

  putStrLn "// initial seed: \{show cfg.randomSeed}"
  let vals = unGenTryAll' cfg.randomSeed $
               genModules cfg.modelFuel StdModules >>= map (render cfg.layoutOpts) . prettyModules (limit 1000) StdModulesPV
  let vals = flip mapMaybe vals $ \gmd => snd gmd >>= \md : String => if nonTrivial md then Just (fst gmd, md) else Nothing
  let vals = take (limit cfg.testsCnt) vals

  case cfg.testsDir of
    Nothing => do
      Lazy.for_ vals $ \(seed, generatedModule) => do
      putStrLn "-------------------\n"
      putStr $ generatedModule ++ "// seed after: \{show seed}\n"
    Just path => do
      -- create dirs
      0 <- system "mkdir -p \{path}"
        | errcode => die "`mkdir` returned \{show errcode}"
      -- set file name paddings
      let padding = countDigit cfg.testsCnt
      let (seeds, modules) = unzip vals
      let alignedSeeds = cfg.randomSeed::seeds
      let numberedVals = withIndex $ zip modules alignedSeeds
      -- print files
      Lazy.for_ numberedVals $ \(idx, (generatedModule, seed)) => do
        let fileName = "\{path}/\{padLeft padding '0' (show idx)}-\{show seed}.sv"
        writeRes <- writeFile fileName generatedModule
        case writeRes of
          Left err => putStrLn (show err)
          Right () => putStrLn "[+] Printed file \{fileName}"
        pure ()
