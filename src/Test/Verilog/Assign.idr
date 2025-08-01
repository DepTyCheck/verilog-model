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
  |||
  ||| So unconnected sumbodule inputs and unconnected top outputs are available for singledriven continuous assignment
  export
  portsToAssign : (mcs : MultiConnectionVect n ms m subMs uf) -> FinsList n
  portsToAssign mcs = fromList $ foldl resolve [] $ withIndex $ toVect mcs where
    resolve : List (Fin n) -> (Fin n, ShortConn ms m subMs) -> List (Fin n)
    resolve acc (fin, MkSC sk sc) = if isUnconnected sc then fin :: acc else acc

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
  data CanDriveMD : (mcs : MultiConnectionVect n ms m subMs uf) -> Fin n -> Type where
    Can : Multidriven (typeOf mcs f) -> CanDriveMD mcs f

  public export
  data MDAssigns : (mcs : MultiConnectionVect n ms m subMs uf) -> Type where 
    Nil  : MDAssigns mcs
    (::) : {mcs : MultiConnectionVect n ms m subMs uf} -> (f : Fin n) -> {_ : CanDriveMD mcs f} -> MDAssigns mcs -> MDAssigns mcs
  
  export
  genMDAssigns : Fuel -> {n : Nat} -> {ms : ModuleSigsList} -> {m : ModuleSig} -> {subMs : FinsList ms.length} -> {uf : UseFins ms m subMs} -> 
                 (mcs : MultiConnectionVect n ms m subMs uf) -> Gen MaybeEmpty $ MDAssigns mcs
