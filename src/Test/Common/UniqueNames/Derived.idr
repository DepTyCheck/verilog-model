module Test.Common.UniqueNames.Derived

import public Test.Common.UniqueNames

import Deriving.DepTyCheck.Gen
import System.Random.Pure.StdGen

%default total

%logging "deptycheck.derive" 7

Test.Common.UniqueNames.rawNewName' = deriveGen
