module Test.VHDL.VHDLDesign

import Data.Fuel

import Test.Common.Utils

import Test.Common.DataType
import Test.Common.Design

import Test.VHDL.Defaults

import Test.DepTyCheck.Gen

public export
data VHDLDesign : DesignUnitSigsList VHDL -> Type where
  End : VHDLDesign dus
  New :
    (u : DesignUnitSig VHDL) ->
    {uu : _} ->
    (subUs : FinsList uu.length) ->
    (mcs : MultiConnectionsList VHDL uu u subUs) ->
    (cont : VHDLDesign $ u::uu) ->
    VHDLDesign uu

export
genVHDL : Fuel -> Gen MaybeEmpty $ VHDLDesign StdDesigns
genVHDL x = do
  raw <- genDesignUnits x VHDL StdDesigns
  res <- extend x raw
  pure res where
    extend : Fuel -> {ms : _} -> DesignUnits ms -> Gen MaybeEmpty $ VHDLDesign ms
    extend x End = pure End
    extend x (New u {uu} subUs mcs cont) = do
      contEx <- extend x cont

      pure $ New u {uu} subUs mcs contEx
