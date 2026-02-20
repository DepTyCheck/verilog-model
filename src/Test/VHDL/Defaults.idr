module Test.VHDL.Defaults

import Test.Common.DataType
import Test.Common.Design
import Test.Common.PrintableDesigns

%default total

public export
StdDesigns : DesignUnitSigsList VHDL
StdDesigns = []

public export
StdVHDLPrintable : PrintableDesigns VHDL StdDesigns
StdVHDLPrintable = []
