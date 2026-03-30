module Test.Common.DataType

import Data.Fin
import Data.Vect

import public Test.Verilog.SVType
import public Test.VHDL.VHDLType

%default total

public export
data Lang = SystemVerilog | VHDL

namespace DataType

  public export
  data DataType : Lang -> Type where
    SVT : Nat   -> DataType SystemVerilog
    VHD : VHDLType -> DataType VHDL

  public export
  DataTypesList : Lang -> Type
  DataTypesList l = List (DataType l)
  %name DataTypesList ds

  export
  dtlistLen : {0 l : _} -> (xs : DataTypesList l) -> (ys : DataTypesList l) -> xs.length + ys.length = (xs ++ ys).length
  dtlistLen []        ys = Refl
  dtlistLen (_ :: xs) ys = rewrite dtlistLen xs ys in Refl

  export
  symdtlistLen : {0 l : _} -> {0 a, b : DataTypesList l} -> (0 m : Nat -> Type) -> m (a.length + b.length) -> m ((a ++ b).length)
  symdtlistLen _ v = rewrite sym $ dtlistLen a b in v

  export
  fixDTLVect : {0 a, b: DataTypesList l} -> Vect (a.length + b.length) c -> Vect ((a ++ b).length) c
  fixDTLVect = symdtlistLen $ \n => Vect n c

  export
  fixDTLFin : {0 l : _} -> {0 a, b : DataTypesList l} -> Fin (a.length + b.length) -> Fin ((a ++ b).length)
  fixDTLFin = symdtlistLen Fin

  public export
  Eq (DataType l) where
    (==) (SVT k) (SVT j) = True -- TODO
    (==) (VHD x) (VHD y) = x == y

namespace PortMode

  public export
  data PortMode : Lang -> Type where
    SVP : PortMode SystemVerilog
    VHP : VHDLPortMode -> PortMode VHDL
