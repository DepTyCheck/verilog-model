module Test.Verilog.Ideas

import Data.List
import Data.Vect
import Data.Vect.Quantifiers

import Test.Verilog

%default total

namespace SuccessiveAdditionsIdea

  -- Submodules can be indices to some outer contexts, to split description of a submodule and its instantiation

  data Module : (ins, outs : Nat) -> (unsatOuts : List ?out_of_subm) -> Type where
    Not   : Module 1 1 []
    And   : Module 1 2 []
    Or    : Module 1 2 []
    Nand  : Module 1 2 []

    Empty   : Module is os (replicate os ?each_output_is_unsatisfied)
    NewSub  : (base : Module is os uos) -> (submodule : Module subIs subOs []) -> Module is os (?sub'sOuts::uos)
    NewConn : (base : Module is os (uo::uos)) -> (conn : ?conn_satifsying_uo) -> Module is os ous

    -- maybe, this type have to be indexed by a list of submodules

namespace AlternativeIdea

  data Module : (ins, outs : Nat) -> Type where
    Not   : Module 1 1
    And   : Module 1 2
    Or    : Module 1 2
    Nand  : Module 1 2

    Composite : (subs : Vect subsCnt (ins ** outs ** Module ins outs)) ->
                (connections : All (\(is ** _ ** _) => Vect is ?an_appropriate_output_of_some_submodule) subs) ->
                Module is os

namespace SuccessiveAdditionsFirther

  -- There may be a phantom type of submodules representing an outer world.
  -- It has the same rules for connections -- much more regularity.
  -- But it must be present as a submodule not more than once.
  -- Maybe, it can be done pretty easily by using specialised list for that,
  -- which is indexed by the presence of the world, or which just has alternatives for having and not having the world.

  record ModuleSignature where
    constructor ModuleSig
    name    : String
    inputs  : Nat
    outputs : Nat

  %inline %tcinline
  (.length) : List a -> Nat
  (.length) = length

  %inline %tcinline
  at : (xs : List a) -> Fin (length xs) -> a
  at = index'

  infixr 5 `at`

  data Module : (available  : List ModuleSignature) ->
                (submodules : List $ Fin available.length) ->
                (signature  : ModuleSignature) ->
                (unsatPts   : List $ Either (Fin signature.outputs) (subm ** Fin $ inputs $ available `at` submodules `at` subm)) ->
                Type where
    Empty : Module av [] (ModuleSig nm ins outs) (Left <$> allFins outs)
  --  EmptyZ : Module av [] (ModuleSig nm ins Z) []
  --  EmptyS : Module av [] (ModuleSig nm ins outs) uns -> Module av [] (ModuleSig nm ins $ S outs) (FZ :: map FS uns)
    NewSub : Module av ss sig uns ->
             (sub : Fin av.length) ->
             Module av (sub::ss) sig (?foo (List.allFins $ inputs $ av `at` sub) ++ map (mapSnd (\(_ ** x) => (_ ** ?fooo))) uns)

namespace InductionOnInputs

  -- For each input of an element of a circuit, there must be precisely one wire connecting something to it.
  -- A keen observation is that, if `I` is a set of inputs of all elements and `O` is a set of outputs of all elements,
  -- then such circuits are representable precisely by total functions `I -> O`. Moreover, since `I` and `O` are finite,
  -- we can have an inductive `List`-like definitions isomorphic to such functions.

  -- This encoding is bad in a sense that it cannot adequately reuse subcircuits, but we are yet to formulate a way around it.

  record Module (ins : Nat) (outs : Nat)

  -- Module element is either a primitive block or a subcircuit
  data ModuleElem = And | Or | Not | Nand | Submodule (ins : Nat ** outs : Nat ** Module ins outs)

  -- Fetching number of inputs/outputs of an element
  (.ins) : ModuleElem -> Nat
  (.ins) And = 2
  (.ins) Or = 2
  (.ins) Not = 1
  (.ins) Nand = 2
  (.ins) (Submodule (ins ** _ ** _)) = ins

  (.outs) : ModuleElem -> Nat
  (.outs) And = 1
  (.outs) Or = 1
  (.outs) Not = 1
  (.outs) Nand = 1
  (.outs) (Submodule (_ ** outs ** _)) = outs

  -- By induction on inputs, define wires connecting elements
  data ModuleConn : (ins : Nat) -> (outs : Nat) -> Type where
    Nil : ModuleConn 0 m
    -- Wire connecting output `from` to input `n`
    (::) : (from : Fin m) -> ModuleConn n m -> ModuleConn (S n) m

  record Module (ins : Nat) (outs : Nat) where
    constructor MkModule
    elems : List ModuleElem
    -- We add 'phantom' nodes, corresponding to inputs/outputs of the whole circuit.
    -- Note that input of a circuit requires a phantom output node, and vice versa.
    conn : ModuleConn (outs + foldr (+) 0 (elems <&> (.ins))) (ins + foldr (+) 0 (elems <&> (.outs)))


data ConnectionUsage = Data | NetResolved | NetUnresolved

connUseCase : ConnectionType -> ConnectionUsage
connUseCase Logic = Data
connUseCase Wire  = NetResolved
connUseCase Uwire = NetUnresolved

