module Test.Pretty.Derived

import public Test.Pretty

import Deriving.DepTyCheck.Gen
import System.Random.Pure.StdGen

%default total

%logging "deptycheck.derive" 5

Test.Pretty.rawNewName' = deriveGen
