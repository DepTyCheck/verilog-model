module Test.Verilog.MultiConnection

import public Test.Verilog.Connections

import Data.Vect
import Data.Vect.Extra
import Data.List

public export
data ModuleConns : Type where
  SC : (ms : ModuleSigsList) ->
       (m : ModuleSig) ->
       (subMs : FinsList ms.length) ->
       (sicons : MFinsList (totalInputs {ms} subMs) $ allSrcsLen m ms subMs) ->
       (tocons : MFinsList (m.outsCount)            $ allSrcsLen m ms subMs) ->
       ModuleConns

public export
data MultiConnection : ModuleConns -> Type where
  MC  : (subInps : List $ Fin $ totalInputs {ms} subMs) -> (topOuts : MFin m.outsCount) -> 
        (source : MFin $ allSrcsLen m ms subMs) -> (type : SVObject) -> 
        MultiConnection $ SC ms m subMs sicons tocons

namespace MultiConnectionsVect

  public export
  data MultiConnectionsVect : Nat -> ModuleConns -> Type where
    Nil  : MultiConnectionsVect 0 m
    (::) : MultiConnection m -> MultiConnectionsVect n m -> MultiConnectionsVect (S n) m

  public export
  (.length) : MultiConnectionsVect n m -> Nat
  (.length) []      = 0
  (.length) (x::xs) = S xs.length

  public export
  fromList : List (MultiConnection m) -> (n : Nat ** MultiConnectionsVect n m)
  fromList []      = (0 ** [])
  fromList (x::xs) = do
    let (r ** est) = MultiConnectionsVect.fromList xs
    (S r ** x::est)

  public export
  toList : MultiConnectionsVect l n -> List $ MultiConnection n
  toList []      = []
  toList (x::xs) = x :: toList xs

  public export
  toVect : MultiConnectionsVect l n -> Vect l $ MultiConnection n
  toVect []      = []
  toVect (x::xs) = x :: toVect xs

  public export
  find : (mcs : MultiConnectionsVect l md) -> Fin l -> SVObject
  find ((MC _ _ _ type)::_ ) FZ     = type
  find (_              ::fs) (FS i) = find fs i

parameters (ms : ModuleSigsList) (m : ModuleSig) (subMs : FinsList ms.length) 
           (sicons : MFinsList (totalInputs {ms} subMs) $ allSrcsLen m ms subMs) (tocons : MFinsList (m.outsCount) $ allSrcsLen m ms subMs)
          --  (md : ModuleConns)

  findTopSink : Fin (allSrcsLen m ms subMs) -> MFin m.outsCount
  findTopSink f = find $ toList $ withIndex $ toVect tocons where
    find : List ((Fin m.outsCount), MFin (allSrcsLen m ms subMs)) -> MFin m.outsCount
    find []                       = Nothing
    find ((_, Nothing)::xs)       = find xs
    find ((fsk, (Just fsrc))::xs) = if f == fsrc then Just fsk else find xs
  
  findSubSinks : Fin (allSrcsLen m ms subMs) -> List $ Fin $ totalInputs {ms} subMs
  findSubSinks f = find $ toList $ withIndex $ toVect sicons where
    find : List ((Fin $ totalInputs {ms} subMs), MFin $ allSrcsLen m ms subMs) -> List $ Fin $ totalInputs {ms} subMs
    find []                       = []
    find ((_, Nothing)::xs)       = find xs
    find ((fsk, (Just fsrc))::xs) = if fsrc == f then fsk :: find xs else find xs
  
  isUnpackedList : List (Fin $ totalInputs {ms} subMs) -> Maybe SVObject
  isUnpackedList []      = Nothing
  isUnpackedList (x::xs) = case isUnpacked (typeOf (allInputs {ms} subMs) x) of
    False => isUnpackedList xs
    True  => Just (typeOf (allInputs {ms} subMs) x)
  
  unpOrGiven : (munp : Maybe SVObject) -> SVObject
  unpOrGiven (Just munp) = if isUnpacked munp then munp else defaultNetType
  unpOrGiven Nothing     = defaultNetType
  
  resolveSource : Fin (allSrcsLen m ms subMs) -> Maybe $ MultiConnection $ SC ms m subMs sicons tocons
  resolveSource f with (findSubSinks f) | (findTopSink f)
    resolveSource f | ss | ts with (m.inpsCount > finToNat f)
      resolveSource f | []         | Nothing    | False = Just $ MC [] Nothing    (Just f) $ unpOrGiven $ Just $ typeOf (allSrcs m ms subMs) f
      resolveSource f | []         | Nothing    | True  = Just $ MC [] Nothing    (Just f) $ typeOf (allSrcs m ms subMs) f
      resolveSource f | []         | (Just fts) | False = Just $ MC [] (Just fts) (Just f) $ typeOf (m.outputs) fts
      resolveSource f | []         | (Just fts) | True  = Nothing -- TopSink <- TopSource
      resolveSource f | ss@(x::xs) | Nothing    | False = Just $ MC ss Nothing    (Just f) $ unpOrGiven $ isUnpackedList ss
      resolveSource f | ss@(x::xs) | Nothing    | True  = Just $ MC ss Nothing    (Just f) $ typeOf (allSrcs m ms subMs) f
      resolveSource f | ss@(x::xs) | (Just fts) | False = Just $ MC ss (Just fts) (Just f) $ typeOf (m.outputs) fts
      resolveSource f | ss@(x::xs) | (Just fts) | True  = Nothing -- TopSink <- TopSource P.S. I hope such case is impossible

||| Possible connections:
||| 1. TopInp -> SubInp -- sicons
||| 2. SubOut -> SubInp -- sicons
||| 3. TopInp -> TopOut -- tocons
||| 4. SubOut -> TopOut -- tocons
|||
||| Possible situations when a new item added in MultiConnectionsVect:
||| 1. SubSink <- SubSource
||| 2. SubSink <- SubSource -> SubSink
||| 3. SubSink <- SubSource -> TopSink
||| 4. TopSink <- SubSource
||| 5. SubSink <- TopSource
||| 6. SubSink <- TopSource -> SubSink
||| 7. Nothing <- Source
||| 8. Sink <- Nothing
export
resolveMultiConnections : (m : ModuleConns) -> (n : Nat ** MultiConnectionsVect n m)
resolveMultiConnections md@(SC ms m subMs sicons tocons) = do
  let (_ ** rs) = resolveWithSource 
  let (_ ** rn) = resolveNoSource
  let rsl = toList rs
  let rnl = toList rn
  fromList $ rsl ++ rnl where

    resolveWithSource : (n : Nat ** MultiConnectionsVect n md)
    resolveWithSource = fromList $ catMaybes $ map (resolveSource ms m subMs sicons tocons) $ List.allFins $ (allSrcsLen m ms subMs) -- (\acc,x => resolveSource x :: acc)


    noSourceInps : (Fin $ totalInputs {ms} subMs, MFin $ allSrcsLen m ms subMs) -> 
                   Maybe $ MultiConnection $ SC ms m subMs sicons tocons
    noSourceInps (fsk, Nothing) = Just $ MC [ fsk ] Nothing Nothing $ typeOf (allInputs {ms} subMs) fsk
    noSourceInps (_  , _      ) = Nothing

    noSourceOuts : (Fin m.outsCount, MFin src) -> Maybe $ MultiConnection $ SC ms m subMs sicons tocons
    noSourceOuts (fsk, Nothing) = Just $ MC [] (Just fsk) Nothing $ typeOf m.outputs fsk
    noSourceOuts (_  , _      ) = Nothing

    routine : MFinsList a b -> ((Fin a, MFin b) -> Maybe $ MultiConnection md) -> List $ Maybe $ MultiConnection md
    routine mfin f = toList $ map f $ withIndex $ toVect mfin

    ||| Sink <- Nothing
    resolveNoSource : (n : Nat ** MultiConnectionsVect n (SC ms m subMs sicons tocons))
    resolveNoSource = fromList $ catMaybes $ (routine sicons noSourceInps) ++ (routine tocons noSourceOuts)
