module Test.Verilog.UniqueNames.Derived

import public Test.Verilog.UniqueNames

import Deriving.DepTyCheck.Gen
import System.Random.Pure.StdGen

%default total

%logging "deptycheck.derive" 7

Test.Verilog.UniqueNames.rawNewName' = deriveGen
