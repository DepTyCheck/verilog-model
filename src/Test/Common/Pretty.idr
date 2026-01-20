module Test.Common.Pretty

import Data.Fuel

import Test.Common.Gen
import Test.Verilog.Defaults
import Test.Verilog.Pretty
import Test.VHDL.Pretty

import Test.DepTyCheck.Gen
import Text.PrettyPrint.Bernardy

export
printDesign : {opts : _} -> Fuel -> GenResult l -> Gen0 $ Doc opts
printDesign x (GenSV   design) = prettyModules x StdModulesPV design
printDesign x (GenVHDL design) = prettyDesign x design
