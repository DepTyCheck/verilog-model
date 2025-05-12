module Test.Verilog.ProceduralBlock

import Data.Fuel
import Data.Vect

import public Data.So

import public Test.Verilog.Module
import public Test.Verilog.Expression

import Test.DepTyCheck.Gen
import Test.DepTyCheck.Gen.Coverage

%default total

public export
data MaybeSVType : Type where
  Nothing : MaybeSVType
  Just : SVType -> MaybeSVType


||| Procudaral blocks are described in the section 12 of the IEEE 1800-2023.
||| According to the subsectionsection 12.3, the statement item is one of the following:
||| - blocking assignment
||| - nonblocking_assignment
||| - procedural_continuous_assignment
||| - case_statement
||| - conditional_statement
||| - subroutine_call_statement
||| - disable_statement
||| - event_trigger
||| - loop_statement
||| - jump_statement
||| - par_block
||| - procedural_timing_control_statement
||| - seq_block
||| - wait_statement
||| - procedural_assertion_statement
||| - clocking_drive ;
||| - randsequence_statement
||| - randcase_statement
||| - expect_property_statement
public export
data ProceduralBlock : (ports : PortsList) ->
                       (isCombinatorial : Bool) ->
                       (procAssignedPorts : ListOfPortsIndices ports) ->
                       (contAssignedPorts : ListOfPortsIndices ports) ->
                       (retTy : MaybeSVType) -> Type

public export
data IsAssignableTo : SVType -> SVType -> Type where
  IsAssignableToA : SupremumType x y y -> (Var x) `IsAssignableTo` (Var y)
  IsAssignambleToB : ArrSupremum x y y -> (Arr x) `IsAssignableTo` (Arr y)

public export
data NotInList : (i : IndexInPorts ports) -> (l : ListOfPortsIndices ports) -> Type where
  NotInListA : So (not (i `elem` ports)) -> NotInList i ports

public export
data IsSubblockRetCompatible : MaybeSVType -> MaybeSVType -> Type where
  NothingIsAlwaysCompatible : IsSubblockRetCompatible Nothing x
  ReflexiveComaptibility : IsSubblockRetCompatible (Just x) (Just x)

namespace CombinatorialElseIf

  public export
  data CombinatorialElseIfCase : (procAssignedPorts : ListOfPortsIndices ports) ->
                                 (contAssignedPorts : ListOfPortsIndices ports) ->
                                 (retTy : MaybeSVType) -> Type where
    CombinatorialCaseConstructor : SVExpression caseExprType ports usedPorts ->
                                   IsLogicOrBite caseExprType =>
                                   (caseBlock : ProceduralBlock ports True procAssignedPorts contAssignedPorts caseTy) ->
                                   caseTy `IsSubblockRetCompatible` retTy =>
                                   CombinatorialElseIfCase procAssignedPorts contAssignedPorts retTy

  public export
  data CombinatorialElseIfCaseList : (procAssignedPorts : ListOfPortsIndices ports) ->
                                     (contAssignedPorts : ListOfPortsIndices ports) ->
                                     (retTy : MaybeSVType) -> Type where
    Nil : CombinatorialElseIfCaseList procAssignedPorts contAssignedPorts retTy
    (::) : CombinatorialElseIfCase procAssignedPorts contAssignedPorts retTy ->
           CombinatorialElseIfCaseList procAssignedPorts contAssignedPorts retTy ->
           CombinatorialElseIfCaseList procAssignedPorts contAssignedPorts retTy

namespace NonCombinatorialElseIf

  public export
  data NonCombinatorialElseIfCase : (ports : PortsList) -> (retTy : MaybeSVType) -> Type where
    NonCombinatorialCaseConstructor : {ports : _} -> {usedPorts : _} -> {procAssignedPorts : _} -> {contAssignedPorts : _} ->
                                      SVExpression caseExprType ports usedPorts ->
                                      IsLogicOrBite caseExprType =>
                                      (caseBlock : ProceduralBlock ports _ procAssignedPorts contAssignedPorts caseTy) ->
                                      caseTy `IsSubblockRetCompatible` retTy =>
                                      NonCombinatorialElseIfCase ports retTy

  public export
   data NonCombinatorialElseIfCaseList : (ports : PortsList) -> (retTy : MaybeSVType) -> Type where
    Nil : NonCombinatorialElseIfCaseList ports retTy
    (::) : {ports : _} ->
           NonCombinatorialElseIfCase ports retTy ->
           NonCombinatorialElseIfCaseList ports retTy ->
           NonCombinatorialElseIfCaseList ports retTy

  public export
  getAllProcAssignments : {ports : _} -> NonCombinatorialElseIfCaseList ports retTy -> ListOfPortsIndices ports
  getAllProcAssignments Nil = []
  getAllProcAssignments ((NonCombinatorialCaseConstructor {procAssignedPorts} _ _) :: xs) = procAssignedPorts `union` (getAllProcAssignments xs)

  public export
  gettAllContAssignments : {ports : _} -> NonCombinatorialElseIfCaseList ports retTy -> ListOfPortsIndices ports
  gettAllContAssignments Nil = []
  gettAllContAssignments ((NonCombinatorialCaseConstructor {contAssignedPorts} _ _) :: xs) = contAssignedPorts `union` (gettAllContAssignments xs)


public export
data ProceduralBlock : (ports : PortsList) ->
                       (isCombinatorial : Bool) ->
                       (procAssignedPorts : ListOfPortsIndices ports) ->
                       (contAssignedPorts : ListOfPortsIndices ports) ->
                       (retTy : MaybeSVType) -> Type where

  BlockingAssignment : (i : IndexInPorts ports) -> (expr : SVExpression expType ports usedPorts) ->
                       AtIndexInPorts i portType => expTpype `IsAssignableTo` portType =>
                       (continuation : ProceduralBlock ports isCombinatorial procAssignedPorts contAssignedPorts retTy) ->
                       NotInList i contAssignedPorts =>
                       ProceduralBlock ports isCombinatorial (i :: procAssignedPorts) contAssignedPorts retTy

  ContinuousAssignment : (i : IndexInPorts ports) -> (expr : SVExpression expType ports usedPorts) ->
                         AtIndexInPorts i portType => expTpype `IsAssignableTo` portType =>
                         (continuation : ProceduralBlock ports isCombinatorial procAssignedPorts contAssignedPorts retTy) ->
                         NotInList i procAssignedPorts =>
                         NotInList i contAssignedPorts =>
                         ProceduralBlock ports isCombinatorial procAssignedPorts (i :: contAssignedPorts) retTy

  CombinatorialIf : {finalRetType : MaybeSVType} ->
                    IsLogicOrBite baseExprType =>
                    {baseProcAssignedPorts :  _} -> {baseContAssignedPorts :  _} ->
                    (baseExpr : SVExpression baseExprType ports usedPorts) ->
                    retTy `IsSubblockRetCompatible` finalRetType =>
                    (baseBlock : ProceduralBlock ports True baseProcAssignedPorts baseContAssignedPorts retTy) ->
                    retTy' `IsSubblockRetCompatible` finalRetType =>
                    (elseBlock : ProceduralBlock ports True baseProcAssignedPorts baseContAssignedPorts retTy') ->
                    (additinalCases : CombinatorialElseIfCaseList baseProcAssignedPorts baseContAssignedPorts finalRetType) ->
                    {continuationProcAssignedPorts : _} -> {continuationContAssignedPorts : _} ->
                    (continuation : ProceduralBlock ports _ continuationProcAssignedPorts continuationContAssignedPorts finalRetType) ->
                    {default (baseProcAssignedPorts `union` continuationProcAssignedPorts) finalProcAssignedPorts : ListOfPortsIndices ports} ->
                    So (finalProcAssignedPorts `setEqual` (baseProcAssignedPorts `union` continuationProcAssignedPorts)) =>
                    {default (baseContAssignedPorts `union` continuationContAssignedPorts) finalContAssignedPorts : ListOfPortsIndices ports} ->
                    So (finalContAssignedPorts `setEqual` (baseContAssignedPorts `union` continuationContAssignedPorts)) =>
                    ProceduralBlock ports _ finalProcAssignedPorts finalContAssignedPorts finalRetType

  NonCombinatorialIf : {finalRetType : MaybeSVType} ->
                       IsLogicOrBite baseExprType =>
                       {baseProcAssignedPorts :  _} -> {baseContAssignedPorts :  _} ->
                       (baseExpr : SVExpression baseExprType ports usedPorts) ->
                       retTy `IsSubblockRetCompatible` finalRetType =>
                       (baseBlock : ProceduralBlock ports _ baseProcAssignedPorts baseContAssignedPorts retTy) ->
                       {elseProcAssignedPorts : _} -> {elseContAssignedPorts : _} ->
                       (elseBlock : ProceduralBlock ports _ elseProcAssignedPorts elseContAssignedPorts retTy') ->
                       retTy' `IsSubblockRetCompatible` finalRetType =>
                       (additinalCases : NonCombinatorialElseIfCaseList ports finalRetType) ->
                       {continuationProcAssignedPorts : _} -> {continuationContAssignedPorts : _} ->
                       (continuation : ProceduralBlock ports _ continuationProcAssignedPorts continuationContAssignedPorts finalRetType) ->
                       {default (((baseProcAssignedPorts `union` elseProcAssignedPorts) `union` (getAllProcAssignments additinalCases)) `union` continuationProcAssignedPorts) finalProcAssignedPorts : ListOfPortsIndices ports} ->
                       So (finalProcAssignedPorts `setEqual` (((baseProcAssignedPorts `union` elseProcAssignedPorts) `union` (getAllProcAssignments additinalCases)) `union` continuationProcAssignedPorts)) =>
                       {default (((baseContAssignedPorts `union` elseContAssignedPorts) `union` (gettAllContAssignments additinalCases)) `union` continuationContAssignedPorts) finalContAssignedPorts : ListOfPortsIndices ports} ->
                       So (finalContAssignedPorts `setEqual` (((baseContAssignedPorts `union` elseContAssignedPorts) `union` (gettAllContAssignments additinalCases)) `union` continuationContAssignedPorts)) =>
                       ProceduralBlock ports False finalProcAssignedPorts finalContAssignedPorts finalRetType

  NonCombinatorialIfNoElse : {finalRetType : MaybeSVType} ->
                             IsLogicOrBite baseExprType =>
                             {baseProcAssignedPorts :  _} -> {baseContAssignedPorts :  _} ->
                             (baseExpr : SVExpression baseExprType ports usedPorts) ->
                             retTy `IsSubblockRetCompatible` finalRetType =>
                             (baseBlock : ProceduralBlock ports _ baseProcAssignedPorts baseContAssignedPorts retTy) ->
                             (additinalCases : NonCombinatorialElseIfCaseList ports finalRetType) ->
                             {continuationProcAssignedPorts : _} -> {continuationContAssignedPorts : _} ->
                             (continuation : ProceduralBlock ports _ continuationProcAssignedPorts continuationContAssignedPorts finalRetType) ->
                             {default ((baseProcAssignedPorts `union` (getAllProcAssignments additinalCases)) `union` continuationProcAssignedPorts) finalProcAssignedPorts : ListOfPortsIndices ports} ->
                             So (finalProcAssignedPorts `setEqual` ((baseProcAssignedPorts `union` (getAllProcAssignments additinalCases)) `union` continuationProcAssignedPorts)) =>
                             {default ((baseContAssignedPorts `union` (gettAllContAssignments additinalCases)) `union` continuationContAssignedPorts) finalContAssignedPorts : ListOfPortsIndices ports} ->
                             So (finalContAssignedPorts `setEqual` ((baseContAssignedPorts `union` (gettAllContAssignments additinalCases)) `union` continuationContAssignedPorts)) =>
                             ProceduralBlock ports False finalProcAssignedPorts finalContAssignedPorts finalRetType

  WhileBlock : IsLogicOrBite exprType =>
               (expr : SVExpression exprType ports usedPorts) ->
               {innerProcAssignedPorts : _} ->
               (innerBlock : ProceduralBlock ports isCombinatorial innerProcAssignedPorts [] Nothing) ->
               {continuationProcAssignedPorts : _} -> {continuationContAssignedPorts : _} ->
               (continuation : ProceduralBlock ports isCombinatorial continuationProcAssignedPorts continuationContAssignedPorts Nothing) ->
               {default (innerProcAssignedPorts `union` continuationProcAssignedPorts) finalProcAssignedPorts : ListOfPortsIndices ports} ->
               So (finalProcAssignedPorts `setEqual` (innerProcAssignedPorts `union` continuationProcAssignedPorts)) =>
               ProceduralBlock ports isCombinatorial finalProcAssignedPorts continuationContAssignedPorts retTy

  ForBlock : {innerExpressionType : _} ->
             (innerAssignment : SVExpression innerExpressionType ports usedPorts) ->
             {0 usedPorts' : ListOfPortsIndices (innerExpressionType :: ports)} ->
             Here `IsElemOf` usedPorts' =>
             (conditionalExpression : SVExpression conditionalExpressionType (innerExpressionType :: ports) usedPorts') ->
             IsLogicOrBite conditionalExpressionType =>
             {0 usedPorts'' : ListOfPortsIndices (innerExpressionType :: ports)} ->
             (stepExpression : SVExpression stepExpressionType (innerExpressionType :: ports) usedPorts'') ->
             {innerProcAssignedPorts : ListOfPortsIndices (innerExpressionType :: ports)} ->
             (innerBlock : ProceduralBlock (innerExpressionType :: ports) isCombinatorial innerProcAssignedPorts [] Nothing) ->
             {continuationProcAssignedPorts : ListOfPortsIndices ports} ->
             (continuation : ProceduralBlock ports isCombinatorial continuationProcAssignedPorts continuationContAssignedPorts retTy) ->
             {default ((shortenIndexPorts innerProcAssignedPorts) `union` continuationProcAssignedPorts) finalProcAssignedPorts : ListOfPortsIndices ports} ->
             So (finalProcAssignedPorts `setEqual` ((shortenIndexPorts innerProcAssignedPorts) `union` continuationProcAssignedPorts)) =>
             ProceduralBlock ports isCombinatorial finalProcAssignedPorts continuationContAssignedPorts retTy

  Return : (exprWithPorts : (usedPorts : _ ** SVExpression expType ports usedPorts)) ->
           expType `IsAssignableTo` retTy =>
           ProceduralBlock ports isCombinatorial [] [] (Just retTy)

  End : ProceduralBlock ports isCombinatorial [] [] Nothing

export
genProceduralBlock : Fuel ->
                     (ports : PortsList) ->(isCombinatorial : Bool) ->
                     (procAssignedPorts : ListOfPortsIndices ports) ->
                     (contAssignedPorts : ListOfPortsIndices ports) ->
                     (retTy : MaybeSVType) ->
                     (Fuel -> (expressionType : SVType) -> (ports : PortsList) -> (usedPorts : ListOfPortsIndices ports) -> Gen MaybeEmpty (SVExpression expressionType ports usedPorts)) =>
                     Gen MaybeEmpty (ProceduralBlock ports isCombinatorial procAssignedPorts contAssignedPorts retTy)
