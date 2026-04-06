module Test.Verilog.SVDesign

import Data.Fuel

import Test.Common.Utils

import Test.Common.UniqueFins
import Test.Common.DataType
import Test.Common.Design

import Test.Verilog.Assign
import Test.Verilog.Literal
import Test.Verilog.TMPExpression
import Test.Verilog.Defaults

import Test.DepTyCheck.Gen

%default total

public export
data SVDesign : DesignUnitSigsList SystemVerilog -> Type where
  End : SVDesign dus
  New : {s : _} -> {usl : _} -> {subUs : _} -> {mcs : _} ->
        (basic : DesignUnit {l=SystemVerilog} s usl subUs mcs) -> 
        (sdAssigns : FinsList $ length mcs) ->
        (sdExprs : TMPExList mcs sdAssigns) ->
        (mdAssigns : FinsList $ length mcs) ->
        (mdExprs : TMPExList mcs mdAssigns) ->
        (cont : SVDesign $ s::usl) ->
        SVDesign usl

export
genSV : Fuel -> Gen MaybeEmpty $ SVDesign StdModules
genSV x = do
  raw <- genDesignUnitsList x SystemVerilog StdModules
  res <- extend x raw
  pure res where
    extend : Fuel -> {dus : _} -> DesignUnitsList dus -> Gen MaybeEmpty $ SVDesign dus
    extend x [] = pure End
    extend x (design@(MkDesign s subUs mcs)::cont) = do
      contEx <- extend x cont

      -- Gen Assigns
      let sdf = sdFins $ allFins $ length mcs
      (rawSDAssigns ** sduf) <- genUF x $ sdf.length
      let sdAssigns = listLookUp sdf rawSDAssigns

      let mdf = mdFins $ allFins $ length mcs
      (rawMDAssigns ** mduf) <- genUF x $ mdf.length
      let mdAssigns = listLookUp mdf rawMDAssigns

      -- Gen Expressions
      sdExprs <- genTMPExList x mcs sdAssigns
      mdExprs <- genTMPExList x mcs mdAssigns

      pure $ New design sdAssigns sdExprs mdAssigns mdExprs contEx
