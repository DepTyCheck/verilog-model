module Test.Verilog.Assign

import public Test.Verilog.SVType
import public Test.Verilog.Connections
import public Test.Verilog.MultiConnection

import Data.Fuel
import Data.Vect.Extra
import Data.Fin

import Test.DepTyCheck.Gen
import Test.DepTyCheck.Gen.Coverage

%default total

namespace SD

  ||| 10.3.2
  ||| Continuous assignments to singledriven types are illegal when assigned to top input ports and submodule output ports
  |||
  ||| So unconnected sumbodule inputs and unconnected top outputs are available for singledriven continuous assignment
  export
  portsToAssign : {m : ModuleConns} -> MultiConnectionsVect l m -> FinsList l
  portsToAssign v = do
    let (_ ** res) = catMaybes $ map resolve $ withIndex $ toVect v
    fromVect res where
      noSource : {m : ModuleConns} -> MultiConnection m -> Bool
      noSource {m = (SC _ _ _ _ _)} (MC _ _ Nothing _) = True
      noSource _                                       = False

      resolve : {m : ModuleConns} -> (Fin l, MultiConnection m) -> Maybe (Fin l)
      resolve (fsk, fss) = if noSource fss then Just fsk else Nothing  

  public export
  data UniqueFins : (n : Nat) -> (fs : FinsList n) -> Type where 
    Nil  : UniqueFins n []
    (::) : (f : Fin n) -> FinNotIn rest f => UniqueFins n rest -> UniqueFins n (f::rest)

  export
  genUniqueFins : Fuel -> (n : Nat) -> Gen MaybeEmpty $ (fs : FinsList n ** UniqueFins n fs)

namespace MD

  ||| 10.3.2
  ||| Nets can be driven by multiple continuous assignments or by a mixture of primitive outputs, module outputs,
  ||| and continuous assignments.
  ||| IEEE 1800-2023
  public export
  data Multidriven : SVObject -> Type where
    RN : ResolvedNet sv => Multidriven sv

  public export
  data CanDriveMD : {m : ModuleConns} -> (mcs : MultiConnectionsVect l m) -> Fin l -> Type where
    Can : Multidriven (find mcs f) -> CanDriveMD mcs f

  public export
  data MDAssigns : {m : ModuleConns} -> (mcs : MultiConnectionsVect l m) -> Type where 
    Nil  : MDAssigns mcs
    (::) : {mcs : MultiConnectionsVect l m} -> (f : Fin l) -> CanDriveMD mcs f => MDAssigns mcs -> MDAssigns mcs
  
  export
  toFinsList : {mcs : MultiConnectionsVect l m} -> MDAssigns mcs -> FinsList l
  toFinsList []      = []
  toFinsList (x::xs) = x :: toFinsList xs

  export
  genMDAssigns : Fuel -> {l : Nat} -> {m : ModuleConns} -> (mcs : MultiConnectionsVect l m) -> 
    Gen MaybeEmpty $ MDAssigns {l} {m} mcs
