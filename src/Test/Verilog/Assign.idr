module Test.Verilog.Assign

import public Test.Common.DataType
import public Test.Common.Design

import Data.Fin

%default total

||| 10.3.2
||| Continuous assignments to singledriven types are illegal when assigned to top input ports and submodule output ports
export
sdFins : {s : _} -> {usl : _} -> {subUs : _} -> {mcs : MultiConnectionsList SystemVerilog s usl subUs} -> 
         List (Fin $ length mcs) -> FinsList (length mcs)
sdFins []      = []
sdFins (x::xs) = if noSource (index mcs x) && isSD (dtToSVt $ typeOf $ index mcs x) && notInoutUwire then x :: sdFins xs else sdFins xs where
  notUwire : Bool
  notUwire = case dtToSVt $ typeOf $ index mcs x of
    Net Uwire' _ => False
    _            => True

  notInout : Bool
  notInout = case index mcs x of
    MkMC (Just f) _ _ _ @{JustTSC} @{OnlyTSC} => case topPortMode s f of
      SVP InOut => False
      _         => True
    MkMC _ (Just f) _ _ @{JustTSK} @{OnlyTSK} => case topPortMode s f of
      SVP InOut => False
      _         => True
    _ => True

  -- Does inout uwire is self driven?
  notInoutUwire : Bool
  notInoutUwire = notUwire && notInout

export
mdFins : {s : _} -> {usl : _} -> {subUs : _} -> {mcs : MultiConnectionsList SystemVerilog s usl subUs} -> 
         List (Fin $ length mcs) -> FinsList (length mcs)
mdFins []      = []
mdFins (x::xs) = if isMDSV (dtToSVt $ typeOf $ index mcs x) then x :: mdFins xs else mdFins xs
