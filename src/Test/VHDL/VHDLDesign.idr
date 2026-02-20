module Test.VHDL.VHDLDesign

import Data.Fuel

import Test.Common.Utils

import Test.Common.DataType
import Test.Common.Design

import Test.VHDL.Defaults

import Test.DepTyCheck.Gen

%default total

public export
data VHDLDesign : DesignUnitSigsList VHDL -> Type where
  End : VHDLDesign dus
  New : {s : _} -> {usl : _} -> {subUs : _} -> {mcs : _} -> 
        (basic : DesignUnit {l=VHDL} s usl subUs mcs) -> (cont : VHDLDesign $ s::usl) -> VHDLDesign usl

export
genVHDL : Fuel -> Gen MaybeEmpty $ VHDLDesign StdDesigns
genVHDL x = do
  raw <- genDesignUnitsList x VHDL StdDesigns
  res <- extend x raw
  pure res where
    extend : Fuel -> {dus : _} -> DesignUnitsList dus -> Gen MaybeEmpty $ VHDLDesign dus
    extend x [] = pure End
    extend x (design::cont) = do
      contEx <- extend x cont

      pure $ New design contEx
