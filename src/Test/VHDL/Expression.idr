module Test.VHDL.Expression

import public Test.Common.Multiconnection

import public Test.VHDL.Literal

||| 6.5.2 Interface object declarations
||| — in. The value of the interface object is allowed to be read, but it shall not be updated by a simple
||| waveform assignment, a conditional waveform assignment, a selected waveform assignment, a
||| concurrent signal assignment, or a variable assignment. Reading an attribute of the interface object is
||| allowed, unless the interface object is a signal parameter and the attribute is one of 'STABLE,
||| 'QUIET, 'DELAYED, 'TRANSACTION, 'DRIVING, or 'DRIVING_VALUE.
||| — out. The value of the interface object is allowed to be updated and, provided it is not a signal
||| parameter, read. Reading the attributes of the interface object is allowed, unless the interface object
||| is a signal parameter and the attribute is one of 'STABLE, 'QUIET, 'DELAYED, 'TRANSACTION,
||| 'EVENT, 'ACTIVE, 'LAST_EVENT, 'LAST_ACTIVE, or 'LAST_VALUE.
||| — inout or buffer. Reading and updating the value of the interface object is allowed. Reading the
||| attributes of the interface object, other than the attributes 'STABLE, 'QUIET, 'DELAYED, and
||| 'TRANSACTION of a signal parameter, is also permitted.
||| — linkage. Reading and updating the value of the interface object is allowed, but only by appearing as
||| an actual corresponding to an interface object of mode linkage. No other reading or updating is
||| permitted.
||| IEEE 1076-2019
public export
canBeTargetUsual : VHDLPortMode -> Bool
canBeTargetUsual In      = False
canBeTargetUsual Out     = True
canBeTargetUsual InOut   = True
canBeTargetUsual Buffer  = True
canBeTargetUsual Linkage = False -- Only connected with Linkage

||| See `canBeTarget` docs
public export
canBeSourceUsual : VHDLPortMode -> Bool
canBeSourceUsual In      = True
canBeSourceUsual Out     = True
canBeSourceUsual InOut   = True
canBeSourceUsual Buffer  = True
canBeSourceUsual Linkage = False

public export
realPortMode : (mcs : MultiConnectionsList VHDL s usl subUs) -> (target : Fin mcs.length) -> Maybe VHDLPortMode
realPortMode mcs f = case index mcs f of
  (MkMC (Just x) Nothing  ssc ssk {ne} {oot = OnlyTSC}) => Just $ pmToVHp (index s.ports x).mode
  (MkMC Nothing  (Just x) ssc ssk {ne} {oot = OnlyTSK}) => Just $ pmToVHp (index s.ports x).mode
  (MkMC Nothing  Nothing  ssc ssk {ne} {oot = NoTop})   => Nothing

public export
canAssign : (target : Maybe VHDLPortMode) -> (source : Maybe VHDLPortMode) -> Bool
canAssign Nothing  Nothing  = True
canAssign Nothing  (Just x) = canBeSourceUsual x
canAssign (Just x) Nothing  = canBeTargetUsual x
canAssign (Just x) (Just y) = canBeTargetUsual x && canBeSourceUsual y

public export
data VHDLExpression : (mcs : MultiConnectionsList VHDL s usl subUs) -> (target : Fin mcs.length) -> Type where
  MkQualName : (fSource : Fin $ length mcs) ->
               So ((valueOfFin mcs fTarget) == (valueOfFin mcs fSource)) =>
               So (canAssign (realPortMode mcs fTarget) (realPortMode mcs fSource)) =>
               VHDLExpression {s} {usl} {subUs} mcs fTarget
  MkLiteral  : VHDLLiteral (valueOfFin mcs fTarget) -> VHDLExpression {s} {usl} {subUs} mcs fTarget
