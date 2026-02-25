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
  data DataTypesList : Lang -> Type where
    Nil  : DataTypesList l
    (::) : DataType l -> DataTypesList l -> DataTypesList l

  %name DataTypesList ds

  public export
  (.asList) : DataTypesList l -> List $ DataType l
  (.asList) []      = []
  (.asList) (x::xs) = x :: xs.asList

  public export
  length : DataTypesList l -> Nat
  length []      = Z
  length (_::usl) = S $ length usl

  public export %inline
  (.length) : DataTypesList l -> Nat
  (.length) = length

  public export
  index : (fs : DataTypesList s) -> Fin fs.length -> DataType s
  index (f::_ ) FZ     = f
  index (_::fs) (FS i) = index fs i

  public export
  (++) : DataTypesList l -> DataTypesList l -> DataTypesList l
  Nil       ++ ys = ys
  (x :: xs) ++ ys = x :: (xs ++ ys)

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

namespace PortMode

  public export
  data PortMode : Lang -> Type where
    SVP : PortMode SystemVerilog
    VHP : VHDLPortMode -> PortMode VHDL
