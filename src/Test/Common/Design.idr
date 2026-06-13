module Test.Common.Design

import Data.Fin
import Data.Fuel
import Data.Vect

import Test.DepTyCheck.Gen

import public Test.Common.UniqueFins
import public Test.Common.Multiconnection
import public Test.Common.Assign

%default total

namespace DesignUnit

  public export
  data DesignUnit : {l : _} ->
                    (s : DesignUnitSig l) ->
                    (usl : DesignUnitSigsList l) ->
                    (subUs : FinsList usl.length) ->
                    (mcs : MultiConnectionsList l s usl subUs) -> Type where
    MkDesign : (s : DesignUnitSig l) ->
               {usl : _} ->
               (subUs : FinsList usl.length) ->
               (mcs : MultiConnectionsList l s usl subUs) ->
               {0 fs : FillSub l s usl subUs (buildTopMCS l s usl subUs) (totalSubs' usl subUs) mcs} ->
               (sdAssigns : FinsList $ length mcs) ->
               {0 sdu : UniqueFinsLookUp (buildSDFins mcs) sdAssigns} ->
               (sdExprs : ExpVect l mcs sdAssigns) ->
               (mdAssigns : FinsList $ length mcs) ->
               {0 mdu : FinsLookUp (buildMDFins mcs) mdAssigns} ->
               (mdExprs : ExpVect l mcs mdAssigns) ->
               DesignUnit {l} s usl subUs mcs

  public export
  data DesignUnitsList : DesignUnitSigsList l -> Type where
    Nil  : DesignUnitsList usl
    (::) : {s : _} -> {usl : _} -> {subUs : _} -> {mcs : _} ->
           DesignUnit {l} s usl subUs mcs -> DesignUnitsList {l} (s::usl) -> DesignUnitsList {l} usl

export
genDesignUnitsList : Fuel -> (l : Lang) -> (usl : DesignUnitSigsList l) ->
  (Fuel -> Gen MaybeEmpty Char) =>
  Gen MaybeEmpty $ DesignUnitsList usl
