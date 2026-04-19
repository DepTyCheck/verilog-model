module Test.Verilog.Defaults

import Test.Common.DataType
import Test.Common.Design
import Test.Common.PrintableDesigns

%default total

Std2Ports = MkDesignUnitSig
  [
    MkPort (SVT $ Var $ AVar Logic') (SVP $ Out)
  , MkPort (SVT $ Var $ AVar Logic') (SVP $ In)
  ]

Std3Ports = MkDesignUnitSig
  [
    MkPort (SVT $ Var $ AVar Logic') (SVP $ Out)
  , MkPort (SVT $ Var $ AVar Logic') (SVP $ In)
  , MkPort (SVT $ Var $ AVar Logic') (SVP $ In)
  ]

public export
StdModules : DesignUnitSigsList SystemVerilog
StdModules =
  [
    Std2Ports
  , Std3Ports
  , Std3Ports
  , Std3Ports
  , Std3Ports
  , Std3Ports
  ]

public export
StdSVPrintable : PrintableDesigns SystemVerilog StdModules
StdSVPrintable = [
    MkPrintableDesign "not" (StdModule 2)
  , MkPrintableDesign "and" (StdModule 3)
  , MkPrintableDesign "or" (StdModule 3)
  , MkPrintableDesign "nand" (StdModule 3)
  , MkPrintableDesign "xor" (StdModule 3)
  , MkPrintableDesign "xnor" (StdModule 3)
  ]
