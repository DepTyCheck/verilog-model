module Test.Common.Gen

import Data.Fuel

import Test.Common.Design
import public Test.Common.DataType

import Test.Verilog.SVDesign
import Test.Verilog.Defaults
import Test.VHDL.VHDLDesign
import Test.VHDL.Defaults

import Test.DepTyCheck.Gen

%default total

public export
data GenResult : Lang -> Type where
  GenSV   : SVDesign   StdModules -> GenResult SystemVerilog
  GenVHDL : VHDLDesign StdDesigns -> GenResult VHDL

export
gen : Fuel -> (l : Lang) -> Gen0 $ GenResult l
gen x SystemVerilog = do
  design <- genSV x
  pure $ GenSV design
gen x VHDL          = do
  design <- genVHDL x
  pure $ GenVHDL design
