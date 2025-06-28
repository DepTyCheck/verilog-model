module Test.Verilog.MultiConnection

import public Test.Verilog.Connections

import Data.Vect
import Data.Vect.Extra

public export
data MultiConnection : {ms : ModuleSigsList} -> Modules ms -> Type where
  NoMC : MultiConnection End
  MkMC  : (subInps : FinsList (totalInputs {ms} subMs)) -> (topOuts : FinsList m.outsCount) -> 
          (source : MFin $ allSrcsLen m ms subMs) -> (type : SVObject) -> 
          MultiConnection $ NewCompositeModule m {ms} subMs {sicons} {tocons} sssi ssto cont

public export
data MultiConnectionsList : Modules ms -> Type where
  Nil  : MultiConnectionsList m
  (::) : MultiConnection m -> MultiConnectionsList m -> MultiConnectionsList m

public export
(++) : MultiConnectionsList m -> MultiConnectionsList m -> MultiConnectionsList m
[]        ++ ys = ys
(x :: xs) ++ ys = x :: (xs ++ ys)

||| Possible connections:
||| 1. TopInp -> SubInp -- sicons
||| 2. SubOut -> SubInp -- sicons
||| 3. TopInp -> TopOut -- tocons
||| 4. SubOut -> TopOut -- tocons
|||
||| Possible situations when a new item added in MultiConnectionsList:
||| 1. SubSink <- SubSource
||| 2. SubSink <- SubSource -> SubSink
||| 3. SubSink <- SubSource -> TopSink
||| 4. TopSink <- SubSource
||| 4. SubSink <- TopSource
||| 2. SubSink <- TopSource -> SubSink
||| 5. Nothing <- Source
||| 6. Sink <- Nothing
export
resolveMultiConnections : {ms : ModuleSigsList} -> (m : Modules ms) -> MultiConnectionsList m
resolveMultiConnections End                                                     = []
resolveMultiConnections md@(NewCompositeModule m subMs {sicons} {tocons} _ _ _) = resolveWithSource 
                                                                               ++ resolveNoSource where

 --     findSks : Fin (srcs.length) -> List (Fin sk, Maybe $ Fin $ srcs.length) -> FinsList sk
--     findSks fss []                   = []
--     findSks fss ((_, Nothing) :: xs) = findSks fss xs
--     findSks fss ((fsk, Just fss') :: xs) with (decEq fss fss')
--       findSks fss ((fsk, Just fss') :: xs) | Yes _ = fsk :: findSks fss xs
--       findSks fss ((fsk, Just fss') :: xs) | No _  = findSks fss xs 

--     fsToMC : Fin (srcs.length) -> MultiConnection allInps allOuts
--     fsToMC fss = do
--       let sis = findSks fss $ toList $ withIndex $ toVect allInps
--       let tos = findSks fss $ toList $ withIndex $ toVect allOuts
--       let t   = typeOf srcs fss
--       MkMultiConnection sis tos t

    findTopSink : Fin (allSrcsLen m ms subMs) -> MFin m.outsCount
    findSubSinks : Fin (allSrcsLen m ms subMs) -> FinsList (totalInputs {ms} subMs)


    resolveSource : Fin (allSrcsLen m ms subMs) -> MultiConnection md
--   resolveSource subOut with (isUnpacked $ typeOf (allOutputs {ms} subMs) subOut)
--     resolveSource subOut | True  = typeOf (allOutputs {ms} subMs) subOut -- The port is unpacked and thus explicitly declared
--     resolveSource subOut | False with (finInMFL tocons $ fixSSFin m ms subMs subOut)
--       resolveSource subOut | False | True = case tryFindTopPort sources tocons (fixSSFin m ms subMs subOut) of -- Source is connected to top output
--         Nothing => defaultNetType -- IMPOSSIBLE CASE. REFACTOR
--         (Just topOutType) => topOutType
--       resolveSource subOut | False | False = defaultNetType -- Source is NOT connected to any sink OR connected to a submodule input

    ||| Situation 1
    resolveWithSource : MultiConnectionsList md
    resolveWithSource = foldl (\acc,x => resolveSource x :: acc) [] $ List.allFins $ (allSrcsLen m ms subMs)




  -- let mcsWithSource = resolveWithSource fs
  -- let mcsNoSource = resolveNoSource
  -- mcsWithSource ++ mcsNoSource 

--     resolveWithSource srcsAllFins = foldl (\acc,finSrc => (fsToMC finSrc) :: acc) [] srcsAllFins

    noSourceInps : MultiConnectionsList md -> (Fin $ totalInputs {ms} subMs, MFin $ allSrcsLen m ms subMs) -> MultiConnectionsList md
    noSourceInps acc (fsk, Nothing) = MkMC [ fsk ] [] Nothing (typeOf (allInputs {ms} subMs) fsk) :: acc
    noSourceInps acc (_  , _      ) = acc

    noSourceOuts : MultiConnectionsList md -> (Fin m.outsCount, MFin $ allSrcsLen m ms subMs) -> MultiConnectionsList md
    noSourceOuts acc (fsk, Nothing) = MkMC [] [ fsk ] Nothing (typeOf m.outputs fsk) :: acc
    noSourceOuts acc (_  , _      ) = acc

    ||| Situation 2
    resolveNoSource : MultiConnectionsList md
    resolveNoSource = foldl noSourceOuts [] (withIndex $ toVect tocons) ++
                      foldl noSourceInps [] (withIndex $ toVect sicons)

    -- unusedSources : List (MFin $ allSrcsLen m ms subMs) -> List $ Fin $ allSrcsLen m ms subMs
    -- unusedSources sources = let lens = List.allFins (allSrcsLen m ms subMs) in foldl resolve [] lens where
    --   contains : List (MFin srcs) -> Fin srcs -> Bool
    --   contains []             f = False
    --   contains (Nothing ::xs) f = contains xs f
    --   contains ((Just x)::xs) f = case x == f of
    --     False => contains xs f
    --     True  => True

    --   resolve : List (Fin $ allSrcsLen m ms subMs) -> Fin (allSrcsLen m ms subMs) -> List $ Fin $ allSrcsLen m ms subMs
    --   resolve acc x = if contains sources x then x::acc else acc

    -- ||| Situation 3
    -- resolveNoSink : MultiConnectionsList md
    -- resolveNoSink = foldl (\acc,x => MkMC [] [] (f2mf x) (typeOf (allSrcs m ms subMs) x) :: acc) [] $ unusedSources $ toList tocons ++ toList sicons
