module Test.Common.Gen

import Data.Fuel

import Test.Common.Design
import public Test.Common.DataType

import Test.Verilog.VerilogDesign
import Test.Verilog.Defaults
import Test.Verilog.Pretty
import Test.VHDL.VHDLDesign
import Test.VHDL.Defaults
import Test.VHDL.Pretty

import Test.DepTyCheck.Gen
import Text.PrettyPrint.Bernardy

%default total

public export
data GenResult : Lang -> Type where
  GenSV   : VerilogDesign StdModules -> GenResult SystemVerilog
  GenVHDL : VHDLDesign    StdDesigns -> GenResult VHDL

export
gen : Fuel -> (l : Lang) -> Gen0 $ GenResult l
gen x SystemVerilog = do
  design <- genSV x
  pure $ GenSV design
gen x VHDL          = do
  design <- genVHDL x
  pure $ GenVHDL design

public export
data PrintMode = HDL | GraphViz

export
printDesign : {opts : _} -> PrintMode -> Fuel -> GenResult l -> Gen0 $ Doc opts
-- printDesign GraphViz x res              = ?knjm 
-- printDesign HDL      x (GenSV   design) = prettyModules x StdModulesPV design
-- printDesign HDL      x (GenVHDL design) = prettyDesign  x StdVHDLPrintable design
