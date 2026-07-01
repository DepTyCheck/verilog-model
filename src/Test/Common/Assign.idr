module Test.Common.Assign

import Data.Fin
import Data.Fuel
import Data.Vect

import Test.DepTyCheck.Gen

import public Test.Common.UniqueFins
import public Test.Common.Multiconnection

import public Test.Verilog.Assign
import public Test.Verilog.Expression

import public Test.VHDL.Assign
import public Test.VHDL.Expression

namespace Expressions

  public export
  data Expr : (l : Lang) -> {s : _} -> {usl : _} -> {subUs : _} -> (mcs : MultiConnectionsList l s usl subUs) ->
              (target : Fin mcs.length) -> Type where
    SV : SVTMPExpression mcs (dtToSVt $ typeOf $ index mcs fTarget) -> Expr SystemVerilog mcs fTarget
    VH : VHDLExpression  mcs fTarget -> Expr VHDL mcs fTarget

  public export
  data ExpVect : (l : Lang) -> {s : _} -> {usl : _} -> {subUs : _} -> (mcs : MultiConnectionsList l s usl subUs) ->
                 FinsList (length mcs) -> Type where
    Nil  : ExpVect l mcs []
    (::) : Expr l mcs f -> ExpVect l mcs fins -> ExpVect l mcs (f::fins)

namespace SD

  public export
  buildSDFins : {l : _} -> {s : _} -> {usl : _} -> {subUs : _} -> (mcs : MultiConnectionsList l s usl subUs) ->
                FinsList mcs.length
  buildSDFins {l = SystemVerilog} mcs = svSDFins mcs
  buildSDFins {l = VHDL}          mcs = vhSDFins mcs

  public export
  data UniqueFinsLookUp : (pre : FinsList n) -> (aft : FinsList n) -> Type where
    MkUFLU : (fins : UniqueFins pre.length uf) -> UniqueFinsLookUp pre (listLookUp pre uf)

namespace MD

  public export
  buildMDFins : {l : _} -> {s : _} -> {usl : _} -> {subUs : _} -> (mcs : MultiConnectionsList l s usl subUs) ->
                FinsList mcs.length
  buildMDFins {l = SystemVerilog} mcs = svMDFins mcs
  buildMDFins {l = VHDL}          mcs = vhMDFins mcs

  public export
  data FinsLookUp : (pre : FinsList n) -> (aft : FinsList n) -> Type where
    MkFLU : (fins : FinsList pre.length) -> FinsLookUp pre (listLookUp pre fins)

namespace Literals

  public export
  charsGen : Fuel -> Gen0 Char
  charsGen _ = choose ('a', 'z')
