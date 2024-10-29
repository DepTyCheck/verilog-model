module Runner

import Data.Fuel
import Data.List.Lazy
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
    MkPrintableModule "and"  ["", ""] [""] STDVerilog
  , MkPrintableModule "or"   ["", ""] [""] STDVerilog
  , MkPrintableModule "nand" ["", ""] [""] STDVerilog
  , MkPrintableModule "xor"  ["", ""] [""] STDVerilog
  , MkPrintableModule "not"  [""]     [""] STDVerilog
  ]

record Config m where
  constructor MkConfig
  randomSeed : m StdGen
  layoutOpts : m LayoutOpts
  testsCnt   : m Nat
  modelFuel  : m Fuel

allNothing : Config Maybe
allNothing = MkConfig
  { randomSeed = Nothing
  , layoutOpts = Nothing
  , testsCnt   = Nothing
  , modelFuel  = Nothing
  }

defaultConfig : IO $ Config Prelude.id
defaultConfig = pure $ MkConfig
  { randomSeed = !initSeed
  , layoutOpts = Opts 152
  , testsCnt   = 10
  , modelFuel  = limit 4
  }

-- TODO to do this with `barbies`
mergeCfg : (forall a. m a -> n a -> k a) -> Config m -> Config n -> Config k
mergeCfg f (MkConfig rs lo tc mf) (MkConfig rs' lo' tc' mf') = MkConfig (f rs rs') (f lo lo') (f tc tc') (f mf mf')

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
               genModules cfg.modelFuel StdModules >>= map (render cfg.layoutOpts) . prettyModules (limit 1000) StdModulesPV ?foo31
  let vals = flip mapMaybe vals $ \gmd => snd gmd >>= \md : String => if nonTrivial md then Just (fst gmd, md) else Nothing
  let vals = vals <&> \(g, d) => d ++ "// seed after: \{show g}\n"
  let vals = take (limit cfg.testsCnt) vals
  Lazy.for_ vals $ \val => do
    putStrLn "-------------------\n"
    putStr val
