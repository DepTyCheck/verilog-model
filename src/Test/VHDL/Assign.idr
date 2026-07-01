module Test.VHDL.Assign

import Data.Fin

import public Test.Common.Multiconnection

%default total

||| Top port of mode in.
isTopInputVHDL : (s : DesignUnitSig VHDL) -> Fin (totalTops' s) -> Bool
isTopInputVHDL s x = topPortMode s x == VHP In

||| Top port of mode linkage.
isTopLinkageVHDL : (s : DesignUnitSig VHDL) -> Fin (totalTops' s) -> Bool
isTopLinkageVHDL s x = topPortMode s x == VHP Linkage

||| 6.5.2 Interface object declarations
||| NOTE 6: A signal port of mode in may be updated by a force assignment, a release assignment, or a call to vhpi_put_value.
||| IEEE 1076-2019
|||
||| So simple assignment to top inputs is forbidden
notTopInput : {s : _} -> {usl : _} -> {subUs : _} ->
              (mc : MultiConnection VHDL s usl subUs) -> Bool
notTopInput (MkMC (Just f) Nothing _ _ @{_} @{OnlyTSC}) = not $ isTopInputVHDL s f
notTopInput (MkMC Nothing (Just f) _ _ @{_} @{OnlyTSK}) = not $ isTopInputVHDL s f
notTopInput (MkMC Nothing Nothing  _ _ @{_} @{NoTop})   = True

||| 10.6.2 Simple variable assignments
||| 10.6.2.1 General
||| It is an error if the type of the target is a protected type or a composite of protected type.
||| IEEE 1076-2019
notProtectedType : {s : _} -> {usl : _} -> {subUs : _} ->
                   (mc : MultiConnection VHDL s usl subUs) -> Bool
notProtectedType mc = case dtToVHt $ typeOf mc of
  Var (Protected _) => False
  _                 => True

||| 6.5.2 Interface object declarations
||| Reading and updating the value of the interface object is allowed, but only by appearing as
||| an actual corresponding to an interface object of mode linkage. No other reading or updating is
||| permitted.
||| IEEE 1076-2019
|||
||| Linkage mode is used by external tools outside VHDL simulator, so no assignments in real designs are allowed
notLinkage : {s : _} -> {usl : _} -> {subUs : _} ->
             (mc : MultiConnection VHDL s usl subUs) -> Bool
notLinkage (MkMC (Just f) Nothing _ _ @{_} @{OnlyTSC}) = not $ isTopLinkageVHDL s f
notLinkage (MkMC Nothing (Just f) _ _ @{_} @{OnlyTSK}) = not $ isTopLinkageVHDL s f
notLinkage (MkMC Nothing Nothing  _ _ @{_} @{NoTop})   = True

||| Single-driven VHDL multiconnections for valid assign
public export
isSDVHDL : {s : _} -> {usl : _} -> {subUs : _} ->
           (mc : MultiConnection VHDL s usl subUs) -> Bool
-- noSource already excludes IN top ports, so no need to double check
isSDVHDL mc = noSource mc && not (isMDVHDL $ valueOfMc mc) && notTopInput mc && notLinkage mc && notProtectedType mc


vhSDFins' : {s : _} -> {usl : _} -> {subUs : _} -> {mcs : MultiConnectionsList VHDL s usl subUs} ->
            List (Fin $ length mcs) -> FinsList (length mcs)
vhSDFins' []      = []
vhSDFins' (x::xs) = if isSDVHDL $ index mcs x then x :: vhSDFins' xs else vhSDFins' xs

public export
vhSDFins : {s : _} -> {usl : _} -> {subUs : _} -> (mcs : MultiConnectionsList VHDL s usl subUs) ->
           FinsList (length mcs)
vhSDFins mcs = vhSDFins' $ allFins mcs.length


||| Multi-driven VHDL multiconnections for valid assign
public export
isMDVHDL : {s : _} -> {usl : _} -> {subUs : _} ->
           (mc : MultiConnection VHDL s usl subUs) -> Bool
isMDVHDL mc = isMDVHDL (valueOfMc mc) && notTopInput mc && notLinkage mc && notProtectedType mc

vhMDFins' : {s : _} -> {usl : _} -> {subUs : _} -> {mcs : MultiConnectionsList VHDL s usl subUs} ->
            List (Fin $ length mcs) -> FinsList (length mcs)
vhMDFins' []      = []
vhMDFins' (x::xs) = if isMDVHDL (index mcs x)
                      then x :: vhMDFins' xs else vhMDFins' xs

public export
vhMDFins : {s : _} -> {usl : _} -> {subUs : _} -> (mcs : MultiConnectionsList VHDL s usl subUs) ->
           FinsList (length mcs)
vhMDFins mcs = vhMDFins' $ allFins mcs.length
