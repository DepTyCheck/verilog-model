module Test.Verilog.Assign

import Data.Fin

import public Test.Common.Multiconnection

%default total

||| 10.3.2
||| Continuous assignments to singledriven types are illegal when assigned to top input ports and submodule output ports
isSDSV : {s : _} -> {usl : _} -> {subUs : _} ->
          (mc : MultiConnection SystemVerilog s usl subUs) -> Bool
isSDSV mc = noSource mc && isSD (dtToSVt $ typeOf mc) && notInoutUwire && notTopInputVar
  where
  notUwire : Bool
  notUwire = case dtToSVt $ typeOf mc of
    Net Uwire' _ => False
    _            => True

  notInout : Bool
  notInout = case mc of
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

  notTopInputVar : Bool
  notTopInputVar = case mc of
    (MkMC (Just f) Nothing ssc ssk @{ne} @{OnlyTSC}) => not $ isTopInputVar s f
    (MkMC Nothing (Just f) ssc ssk @{ne} @{OnlyTSK}) => True
    (MkMC Nothing Nothing  ssc ssk @{ne} @{NoTop})   => True


svSDFins' : {s : _} -> {usl : _} -> {subUs : _} -> {mcs : MultiConnectionsList SystemVerilog s usl subUs} ->
            List (Fin $ length mcs) -> FinsList (length mcs)
svSDFins' []      = []
svSDFins' (x::xs) = if isSDSV $ index mcs x then x :: svSDFins' xs else svSDFins' xs

public export
svSDFins : {s : _} -> {usl : _} -> {subUs : _} -> (mcs : MultiConnectionsList SystemVerilog s usl subUs) ->
           FinsList (length mcs)
svSDFins mcs = svSDFins' $ allFins mcs.length


svMDFins' : {s : _} -> {usl : _} -> {subUs : _} -> {mcs : MultiConnectionsList SystemVerilog s usl subUs} ->
            List (Fin $ length mcs) -> FinsList (length mcs)
svMDFins' []      = []
svMDFins' (x::xs) = if isMDSV (dtToSVt $ typeOf $ index mcs x) then x :: svMDFins' xs else svMDFins' xs

public export
svMDFins : {s : _} -> {usl : _} -> {subUs : _} -> (mcs : MultiConnectionsList SystemVerilog s usl subUs) ->
           FinsList (length mcs)
svMDFins mcs = svMDFins' $ allFins mcs.length
