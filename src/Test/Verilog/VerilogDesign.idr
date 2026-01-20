module Test.Verilog.VerilogDesign

import Data.Fuel

import Test.Common.UniqueFins

import Test.Verilog.SVType
import Test.Verilog.Connections
import Test.Verilog.Assign
import Test.Verilog.Literal
import Test.Verilog.TMPExpression
import Test.Verilog.Defaults

import Test.DepTyCheck.Gen

public export
data VerilogDesign : ModuleSigsList -> Type where

  End : VerilogDesign ms
  ||| A module with assigns and literals
  NewCompositeModule :
    (m : ModuleSig) ->
    (subMs : FinsList ms.length) ->
    (mcs : MultiConnectionsList ms m subMs) ->
    (sdAssigns : FinsList $ length mcs) ->
    (sdExprs : TMPExList mcs sdAssigns) ->
    (mdAssigns : FinsList $ length mcs) ->
    (mdExprs : TMPExList mcs mdAssigns) ->
    (cont : VerilogDesign $ m::ms) ->
    VerilogDesign ms

export
genSV : Fuel -> Gen MaybeEmpty $ VerilogDesign StdModules
genSV x = do
  rawMS <- genModules x StdModules @{genFillAny}
  res <- extend x rawMS
  pure res where
    extend : Fuel -> {ms: _} -> Modules ms -> Gen MaybeEmpty $ VerilogDesign ms
    extend _ End = pure End
    extend x modules@(NewCompositeModule m {ms} subMs {mcs} _ cont) = do
      -- Extend the rest
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

      pure $ NewCompositeModule m subMs mcs sdAssigns sdExprs mdAssigns mdExprs contEx
