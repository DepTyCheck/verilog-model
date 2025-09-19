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

Test.Verilog.Expression.genSVType = deriveGen

Test.Verilog.Expression.genSVObjListGen = deriveGen

Test.Verilog.Expression.genExpressions = deriveGen
--Test.Verilog.Expression.genExpressions' = deriveGen

