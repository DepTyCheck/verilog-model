module Test.Verilog.Pretty

import Data.Either
import Data.List
import Data.List.Extra
import Data.List1
import Data.List.Lazy

import Data.Fin.Split
import Data.Fuel
import Data.SortedMap
import public Data.Vect
import Data.Vect.Extra

import public Test.Verilog

import Test.DepTyCheck.Gen
import System.Random.Pure.StdGen

import Text.PrettyPrint.Bernardy

import Syntax.IHateParens.List
import Syntax.IHateParens.SortedMap

%default total

toTotalInputsIdx : {ms : ContextModuleList} -> {subMs : FinsList ms.length} ->
                   (idx : Fin subMs.asList.length) ->
                   Fin (indexSig ms (index' subMs.asList idx)).inputs ->
                   Fin $ totalInputs {ms} subMs
toTotalInputsIdx {subMs=i::_ } FZ       x = indexSum $ Left x
toTotalInputsIdx {subMs=i::is} (FS idx) x = indexSum $ Right $ toTotalInputsIdx idx x

toTotalOutputsIdx : {ms : ContextModuleList} -> {subMs : FinsList ms.length} ->
                    (idx : Fin subMs.asList.length) ->
                    Fin (indexSig ms $ index' subMs.asList idx).outputs ->
                    Fin $ totalOutputs {ms} subMs
toTotalOutputsIdx {subMs=i::_ } FZ       x = indexSum $ Left x
toTotalOutputsIdx {subMs=i::is} (FS idx) x = indexSum $ Right $ toTotalOutputsIdx idx x

connFwdRel : Connections f t -> Vect t $ Fin f
connFwdRel []      = []
connFwdRel (i::cs) = i :: connFwdRel cs

public export
data SVect : (len : Nat) -> Type where
  ||| Empty vector
  Nil  : SVect Z
  ||| A non-empty vector of length `S len`, consisting of a head element and
  ||| the rest of the list, of length `len`.
  (::) : (x : String) -> (xs : SVect len) -> SVect (S len)

(++) : SVect a -> SVect b -> SVect (a + b)
(++) [] xs = xs
(++) (x :: xs) ys = x :: (xs ++ ys)

(.length) : SVect l -> Nat
(.length) [] = Z
(.length) (x :: xs) = S xs.length

toVect : SVect l -> Vect l String
toVect [] = []
toVect (x :: xs) = x :: toVect xs

public export
fromVect : Vect l String -> SVect l
fromVect [] = []
fromVect (x :: xs) = x :: fromVect xs

public export
data UniqNames : (l : Nat) -> SVect l -> Type
public export
data NameIsNew : (l : Nat) -> (names: SVect l) -> String -> UniqNames l names -> Type

-- Port JustNew
public export
data UniqNames : (l : Nat) -> SVect l -> Type where
  Empty : UniqNames 0 []
  JustNew : {l : Nat} -> (names : SVect l) -> (name: String) -> (un: UniqNames l names) -> NameIsNew l names name un -> UniqNames l names
  Cons : {l : Nat} -> (names : SVect l) -> (name: String) -> (un: UniqNames l names) -> NameIsNew l names name un -> UniqNames (S l) (name :: names)

public export
data NameIsNew : (l : Nat) -> (names: SVect l) -> (name: String) -> UniqNames l names -> Type where
  NNEmpty : NameIsNew 0 [] s Empty
  NNCons : (0 _ : So $ x /= name) -> (nin: NameIsNew l xs x nn) -> NameIsNew (S l) (x :: xs) name (Cons xs x nn nin)

-- Minimize the signature while preserving the issue.
export
rawNewName : Fuel -> (Fuel -> Gen MaybeEmpty String) =>
             (l : Nat) -> (names: SVect l) -> (un: UniqNames l names) -> Gen MaybeEmpty (s ** NameIsNew l names s un)

namesGen : Gen0 String
namesGen = pack <$> listOf {length = choose (1,10)} (choose ('a', 'z'))

namesGen' : Fuel -> Gen MaybeEmpty String
namesGen' _ = namesGen

genOneUniqueName : {l : Nat} -> Fuel -> (names: SVect l) -> (un: UniqNames l names) ->
                   Gen MaybeEmpty (out : String ** UniqNames (S l) (out :: names))
genOneUniqueName x names un = do
  (name ** uname) <- rawNewName x @{namesGen'} l names un
  pure (name ** Cons names name un uname)

genNUniqueNames : {l : Nat} -> Fuel -> (n : Nat) -> (names: SVect l) -> (un: UniqNames l names) ->
                  Gen MaybeEmpty (newNames : SVect (n + l) ** UniqNames (n + l) newNames)
genNUniqueNames _ Z names un = pure (names ** un)
genNUniqueNames x (S k) names un = do
  (tail ** utail) <- genNUniqueNames x k names un
  (head ** uhead) <- genOneUniqueName x tail utail
  pure (head :: tail ** uhead)

invertConn : {ins: Nat} -> Vect outs (Fin ins) -> Vect ins (List (Fin outs))
invertConn = foldl registerDriven (replicate ins []) . withIndex
  where
    registerDriven : Vect ins (List (Fin outs)) -> (Fin outs, Fin ins) -> Vect ins (List (Fin outs))
    registerDriven ys (outIdx, inIdx) = updateAt inIdx (outIdx ::) ys

getFirstTopLevel : {topLevelOutputs: Nat} -> List (Fin (topLevelOutputs + subMInputs)) -> Maybe (Fin topLevelOutputs)
getFirstTopLevel [] = Nothing
getFirstTopLevel (x :: xs) = case isLT (finToNat x) topLevelOutputs of
  Yes a => Just $ natToFinLT $ finToNat x
  No b => getFirstTopLevel xs

-- Finds all inputs that drive no toplevel outputs
findInternalInputs :  {topLevelOutputs: Nat} ->
                      Vect l (Fin ins, List (Fin (topLevelOutputs + subMInputs))) ->
                      List (Fin ins)
findInternalInputs = map fst . filter (isNothing . getFirstTopLevel . snd) . toList

computeInternalsLookupTable : (internals : List (Fin l)) -> (names: Vect (internals.length) String) -> SortedMap (Fin l) String
computeInternalsLookupTable [] [] = empty
computeInternalsLookupTable (x :: xs) (y :: ys) = insert x y $ computeInternalsLookupTable xs ys

solveInputNames : {topLevelInputs: Nat} -> {topLevelOutputs: Nat} -> Vect topLevelInputs String -> Vect topLevelOutputs String ->
                  SortedMap (Fin (topLevelInputs + subMOutputs)) String ->
                  Vect (topLevelInputs + subMOutputs) (Fin (topLevelInputs + subMOutputs), List (Fin (topLevelOutputs + subMInputs))) ->
                  Vect (topLevelInputs + subMOutputs) String
solveInputNames iNames oNames lut = (iNames ++) . map solveName . drop topLevelInputs
  where
    solveName : (Fin (topLevelInputs + subMOutputs), List (Fin (topLevelOutputs + subMInputs))) -> String
    solveName (i, v) = fromMaybe "<error>" $ lookup i lut <|> flip index oNames <$> getFirstTopLevel v

solveOutputNames :  {topLevelOutputs: Nat} -> Vect ins String -> Vect topLevelOutputs String ->
                    Vect (topLevelOutputs + subMInputs) (Fin ins) ->
                    Vect (topLevelOutputs + subMInputs) String
solveOutputNames iNames oNames = (oNames ++) . map (flip index iNames) . drop topLevelOutputs

solveAssigns :  {topLevelOutputs: Nat} -> Vect ins String -> Vect topLevelOutputs String ->
                Vect (topLevelOutputs + subMInputs) (Fin ins) -> List (Fin topLevelOutputs, Fin ins)
solveAssigns iNames oNames = filter namesNotIdentical . toList . withIndex . take topLevelOutputs
  where
    namesNotIdentical : (Fin topLevelOutputs, Fin ins) -> Bool
    namesNotIdentical (outId, inId) = index outId oNames /= index inId iNames

public export
data SigNames : ModuleSig -> Type where
  SNames : Vect m.inputs String -> Vect m.outputs String -> SigNames m

export
prettyModules : {opts : _} -> {ms : ContextModuleList} -> Fuel -> (names : SVect ms.length) -> UniqNames ms.length names => 
                Vect (ms.length) (Maybe (m : ModuleSig ** SigNames m)) ->
                ModuleList ms -> Gen0 $ Doc opts
prettyModules x _ _ End = pure empty
prettyModules x names sigNames @{un} (MCons (MModule m subMs conn) cont) = do
  -- Generate submodule name
  (name ** isnew) <- rawNewName x @{namesGen'} ms.length names un
  -- Generate toplevel input names
  (namesWithInput ** uni) <- genNUniqueNames x m.inputs names un
  let inputNames = take m.inputs $ toVect namesWithInput
  -- Generate toplevel output names
  (namesWithIO ** unio) <- genNUniqueNames x m.outputs namesWithInput uni
  let outputNames = take m.outputs $ toVect namesWithIO
  -- Generate submodule instance names
  (namesIOWithSubMs ** uniosub) <- genNUniqueNames x subMs.length namesWithIO unio
  let subMInstanceNames = take subMs.length $ toVect namesIOWithSubMs

  -- Choose if an attempt to use argument names each submodule instance will be made
  subMInstanceUseArgNames <- vectOf {n=length (subMs.asList)} (choose (False, True))

  -- Extract a output to driving input mapping from conn
  let outputToDriver = connFwdRel conn
  -- Invert it into a input to driven outputs mapping
  let inputToDriven : Vect (m.inputs + totalOutputs subMs) $ List $ Fin $ m.outputs + totalInputs subMs
                    = invertConn outputToDriver

  -- Generate names for internal inputs (inputs that drive no toplevel outputs)
  let internalInputs = findInternalInputs $ withIndex inputToDriven
  (namesWithInternal ** unint) <- genNUniqueNames x internalInputs.length namesIOWithSubMs uniosub
  let internalInputNames = take internalInputs.length $ toVect namesWithInternal

  -- Create a full list of input names
  let internalLUT = computeInternalsLookupTable internalInputs internalInputNames
  let fullInputNames : Vect (m.inputs + totalOutputs subMs) String
                     = solveInputNames inputNames outputNames internalLUT $ withIndex inputToDriven
  let subMONames = drop m.inputs fullInputNames
  -- Create a full list of output names
  let fullOutputNames : Vect (m.outputs + totalInputs subMs) String
                      = solveOutputNames fullInputNames outputNames outputToDriver
  let subMINames = drop m.outputs fullOutputNames
  -- Compute necessary assign statements
  let assigns = solveAssigns fullInputNames outputNames outputToDriver

  -- Recursive call to use at the end
  recur <- prettyModules x (name::names) ((Just (m ** SNames inputNames outputNames))::sigNames) cont
  pure $ vsep
    [ enclose (flush $ line "module" <++> line name) (line "endmodule:" <++> line name) $ flush $ indent 2 $ vsep $ do
      let outerModuleInputs = map ("input logic " ++) inputNames
      let outerModuleOutputs = map ("output logic " ++) outputNames
      let outerModuleIO = toList $ line <$> (outerModuleOutputs ++ outerModuleInputs)
      [ tuple outerModuleIO <+> symbol ';' , line "" ] ++
        (zip (toList subMInstanceNames) (withIndex subMs.asList) <&> \(instanceName, subMsIdx, msIdx) =>
          line (index msIdx $ toVect names) <++> line instanceName <+> do
            let inputs  = List.allFins (indexSig ms $ index' subMs.asList subMsIdx).inputs  <&> toTotalInputsIdx subMsIdx
            let outputs = List.allFins (indexSig ms $ index' subMs.asList subMsIdx).outputs <&> toTotalOutputsIdx subMsIdx

            let inputs  = inputs  <&> flip index subMINames
            let outputs = outputs <&> flip index subMONames

            case (index subMsIdx subMInstanceUseArgNames, index msIdx sigNames) of
              (False, _) => (tuple $ line <$> outputs ++ inputs) <+> symbol ';'
              (_, Nothing) => (tuple $ line <$> outputs ++ inputs) <+> symbol ';'
              (True, Just (_ ** SNames is os)) => do
                let os_strings = zip (toList os) outputs <&> \(oname, cname) => "." ++ oname ++ "(" ++ cname ++ ")"
                let is_strings = zip (toList is) inputs <&> \(iname, cname) => "." ++ iname ++ "(" ++ cname ++ ")"
                (tuple $ line <$> os_strings ++ is_strings) <+> symbol ';'
        ) ++ [line ""] ++ (assigns <&> \(outIdx, inIdx) =>
          line "assign" <++> line (index outIdx outputNames) <++> symbol '=' <++> line (index inIdx fullInputNames) <+> symbol ';'
        )
    , line ""
    , recur
    ]
