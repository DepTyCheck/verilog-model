module Test.Common.Pretty

import Data.Fuel

import Test.Common.Gen
import Test.Verilog.Connections
import Test.Verilog.UniqueNames
import Test.Verilog.PrintableModules
import Test.Verilog.Defaults
import Test.Verilog.Pretty
import Test.VHDL.Pretty

import Test.DepTyCheck.Gen
import Text.PrettyPrint.Bernardy

export
printDesigns : {opts : _} -> Fuel -> GenResult l -> Gen0 $ Doc opts
printDesigns x (GenSV   design) = prettyModules x StdModulesPV design
printDesigns x (GenVHDL design) = prettyDesign x design 
