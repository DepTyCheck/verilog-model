module Test.Verilog.Design

import Data.Fuel

import Test.Common.Utils

import Test.Common.UniqueFins
import Test.Common.DataType
import Test.Common.Design
import Test.Common.Assign

import Test.Verilog.Assign
import Test.Verilog.Literal
import Test.Verilog.Expression
import Test.Verilog.Defaults

import Test.DepTyCheck.Gen

%default total

public export
data SVDesign : DesignUnitSigsList SystemVerilog -> Type where
  End : SVDesign dus
  New : {s : _} -> {usl : _} -> {subUs : _} -> {mcs : _} ->
        (basic : DesignUnit {l=SystemVerilog} s usl subUs mcs) ->
        (cont : SVDesign $ s::usl) ->
        SVDesign usl

export
genSV : Fuel -> Gen MaybeEmpty $ SVDesign StdModules
genSV x = do
  raw <- genDesignUnitsList x SystemVerilog StdModules @{charsGen}
  res <- extend x raw
  pure res where
    extend : Fuel -> {dus : _} -> DesignUnitsList dus -> Gen MaybeEmpty $ SVDesign dus
    extend x []               = pure End
    extend x (design :: cont) = do
      contEx <- extend x cont
      pure $ New design contEx
