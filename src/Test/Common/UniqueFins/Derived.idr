module Test.Common.UniqueFins.Derived

import Deriving.DepTyCheck.Gen

import public Test.Common.UniqueFins

%default total

%logging "deptycheck" 20

Test.Common.UniqueFins.genUF = deriveGen
