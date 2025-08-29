module Test.Verilog.Assign

import public Test.Verilog.SVType
import public Test.Verilog.Connections

import Data.Fuel
import Data.Vect.Extra
import Data.Fin

import Test.DepTyCheck.Gen
import Test.DepTyCheck.Gen.Coverage

%default total

namespace SD

  ||| 10.3.2
  ||| Continuous assignments to singledriven types are illegal when assigned to top input ports and submodule output ports
  public export
  data SDAssigns : (mcs : MultiConnectionsList ms m subMs) -> {fs : FinsList $ length mcs} -> (uf : UniqueFins (length mcs) fs) -> Type where
    SDNil  : SDAssigns mcs uf
    SDCons : {mcs : MultiConnectionsList ms m subMs} -> {old : FinsList $ length mcs} -> {oldUF : UniqueFins (length mcs) old} ->
             (f : Fin $ length mcs) -> 
             (ns : So $ noSource $ index mcs f) => (sd : SingleDriven $ typeOf $ index mcs f) => (newUF : UniqueFins (length mcs) (f::old)) =>
             (rest : SDAssigns mcs oldUF) -> SDAssigns mcs newUF

  export
  genSDAssigns : Fuel -> {ms : ModuleSigsList} -> {m : ModuleSig} -> {subMs : FinsList ms.length} ->
                 (mcs : MultiConnectionsList ms m subMs) -> 
                 Gen MaybeEmpty (fs : FinsList $ length mcs ** (uf : UniqueFins (length mcs) fs ** SDAssigns mcs uf))

namespace MD

  public export
  data MDAssigns : (mcs : MultiConnectionsList ms m subMs) -> Type where
    Nil  : MDAssigns mcs
    (::) : {mcs : MultiConnectionsList ms m subMs} -> (f : Fin $ length mcs) -> Multidriven (typeOf $ index mcs f) => MDAssigns mcs -> MDAssigns mcs

  export
  genMDAssigns : Fuel -> {ms : ModuleSigsList} -> {m : ModuleSig} -> {subMs : FinsList ms.length} ->
                 (mcs : MultiConnectionsList ms m subMs) -> Gen MaybeEmpty $ MDAssigns mcs

  export
  toFinsList : MDAssigns mcs -> FinsList (length mcs)
  toFinsList []      = []
  toFinsList (x::xs) = x :: toFinsList xs