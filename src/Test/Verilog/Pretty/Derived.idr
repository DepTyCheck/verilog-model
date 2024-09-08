module Test.Verilog.Pretty.Derived

import public Test.Verilog.Pretty

import Deriving.DepTyCheck.Gen
import System.Random.Pure.StdGen

%default total

%logging "deptycheck.derive" 5

Test.Verilog.Pretty.rawNewName' = deriveGen
