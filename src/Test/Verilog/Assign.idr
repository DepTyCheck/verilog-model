module Test.Verilog.Assign

import public Test.Verilog.SVType
import public Test.Verilog.Connections

import Data.Fin

%default total

||| 10.3.2
||| Continuous assignments to singledriven types are illegal when assigned to top input ports and submodule output ports
export
sdFins : {mcs : MultiConnectionsList ms m subMs} -> List (Fin $ length mcs) -> FinsList (length mcs)
sdFins []      = []
sdFins (x::xs) = if noSource (index mcs x) && isSD (typeOf $ index mcs x) then x :: sdFins xs else sdFins xs

export
mdFins : {mcs : MultiConnectionsList ms m subMs} -> List (Fin $ length mcs) -> FinsList (length mcs)
mdFins []      = []
mdFins (x::xs) = if isMD (typeOf $ index mcs x) then x :: mdFins xs else mdFins xs
