module Test.Common.DataType

import Data.Fin

import Test.Verilog.SVType
import Test.VHDL.VHDLType

public export
data Lang = SystemVerilog | VHDL

public export
data DataType : Lang -> Type where
  SVT : SVType   -> DataType SystemVerilog
  VHT : VHDLType -> DataType VHDL

public export
data DataTypesList : Lang -> Type where
  Nil  : DataTypesList n
  (::) : DataType n -> DataTypesList n -> DataTypesList n

%name DataTypesList ds

public export
(.asList) : DataTypesList n -> List $ DataType n
(.asList) []      = []
(.asList) (x::xs) = x :: xs.asList

public export
(.length) : DataTypesList n -> Nat
(.length) []      = 0
(.length) (x::xs) = S xs.length

public export
index : (fs : DataTypesList s) -> Fin fs.length -> DataType s
index (f::_ ) FZ     = f
index (_::fs) (FS i) = index fs i
