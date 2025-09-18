module Test.Verilog.Expression.Derived

import Deriving.DepTyCheck.Gen

import Test.DepTyCheck.Gen
import Test.DepTyCheck.Gen.Coverage

import System.Random.Pure.StdGen

import System
import System.Directory

import public Test.Verilog.Expression

import System.Clock

%default total

%logging "deptycheck" 20

genSVType : Fuel -> Gen MaybeEmpty SVType
genSVType = deriveGen

genSVObjListGen : Fuel -> Gen MaybeEmpty SVObjList
genSVObjListGen = deriveGen

--Test.Verilog.Expression.genExpressions = deriveGen
--Test.Verilog.Expression.genExpressions' = deriveGen

dseed : IO StdGen
dseed = initSeed

covering
mapMaybe : (a -> Maybe b) -> Stream a -> Stream b
mapMaybe f (x :: xs) with (f x)
  mapMaybe f (x :: xs) | Nothing = mapMaybe f xs
  mapMaybe f (x :: xs) | (Just y) = y :: mapMaybe f xs

forS_ : Monad f => (seed : s) -> LazyList a -> (s -> a -> f s) -> f ()
forS_ seed []      g = pure ()
forS_ seed (x::xs) g = forS_ !(g seed x) xs g

pathToCovergeInfo : String
pathToCovergeInfo = "./coverage_info.txt"

printMCov : CoverageGenInfo a -> String -> IO ()
printMCov cgi path = do
  Right () <- writeFile path $ show @{Colourful} cgi | Left err => die "Couldn't write the model coverage to file: \{show err}"
  pure ()

covering
test : IO ()
test = do
  osTime <- clockTime UTC
  putStr "Derivation end time : "
  printLn $ show (seconds osTime)
  let intialCoverage = initCoverageInfo'' [`{SVExpression}]
  let vals = unGenTryAllD' !dseed $ genExpressions' (limit 10)
  let vals = flip mapMaybe vals $ \gmd => snd gmd
  let vals = take (limit 5) vals
  let (coverages, exprs) = unzip vals
  forS_ intialCoverage coverages $ \cgi, mcov => do
    let cgi = registerCoverage mcov cgi
    printMCov cgi pathToCovergeInfo
    pure cgi
  osTime <- clockTime UTC
  putStr "Geneation and coverage report end time: "
  printLn $ show (seconds osTime)
  printLn "Coverage report is written to \{pathToCovergeInfo}"
