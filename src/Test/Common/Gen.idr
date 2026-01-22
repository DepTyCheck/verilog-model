module Test.Common.Gen

import Data.Fuel

import Test.Verilog.VerilogDesign
import Test.Verilog.Defaults
import Test.VHDL.VHDLDesign

import Test.DepTyCheck.Gen

public export
data Lang = SystemVerilog | VHDL

public export
data GenResult : Lang -> Type where
  GenSV   : VerilogDesign StdModules -> GenResult SystemVerilog
  GenVHDL : VHDLDesign               -> GenResult VHDL

export
gen : Fuel -> (l : Lang) -> Gen0 $ GenResult l
gen x SystemVerilog = do
  design <- genSV x
  pure $ GenSV design
gen x VHDL          = do
  pure $ GenVHDL genVHDL
