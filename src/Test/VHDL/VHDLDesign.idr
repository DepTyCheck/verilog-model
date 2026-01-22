module Test.VHDL.VHDLDesign

public export
data VHDLDesign : Type where
  MkVHDL : VHDLDesign

export
genVHDL : VHDLDesign
genVHDL = MkVHDL
