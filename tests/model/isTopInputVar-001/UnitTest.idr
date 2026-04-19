module UnitTest

import Test.Common.DataType
import Test.Common.Design
import Test.Common.Utils
import Test.Verilog.SVType

%default total

inVar : DesignUnitSig SystemVerilog
inVar = MkDesignUnitSig [MkPort (SVT $ Var $ AVar Logic') (SVP In)]

inNet : DesignUnitSig SystemVerilog
inNet = MkDesignUnitSig [MkPort (SVT $ Net Wire' (AVar Logic')) (SVP In)]

outVar : DesignUnitSig SystemVerilog
outVar = MkDesignUnitSig [MkPort (SVT $ Var $ AVar Logic') (SVP Out)]

inoutNet : DesignUnitSig SystemVerilog
inoutNet = MkDesignUnitSig [MkPort (SVT $ Net Wire' (AVar Logic')) (SVP InOut)]

||| Assign to var top input is forbidden
topInputVarIsFlagged : isTopInputVar UnitTest.inVar FZ = True
topInputVarIsFlagged = Refl

||| Resolved net input stays multi-drivable
topInputNetNotFlagged : isTopInputVar UnitTest.inNet FZ = False
topInputNetNotFlagged = Refl

||| Only input ports are guarded
topOutputVarNotFlagged : isTopInputVar UnitTest.outVar FZ = False
topOutputVarNotFlagged = Refl

||| Bidirectional vars are allowed
topInoutNetNotFlagged : isTopInputVar UnitTest.inoutNet FZ = False
topInoutNetNotFlagged = Refl
