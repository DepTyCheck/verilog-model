module Test.Verilog.MultiConnection

import public Test.Verilog.Connections

import Data.Vect
import Data.Vect.Extra
import Data.List

-- data SimplifiedConns : Type where
--   SC : (ms : ModuleSigsList) ->
--        (m : ModuleSig) ->
--        (subMs : FinsList ms.length) ->
--        {sicons : MFinsList (totalInputs {ms} subMs) $ allSrcsLen m ms subMs} ->
--        {tocons : MFinsList (m.outsCount)            $ allSrcsLen m ms subMs} ->
--        SimplifiedConns

public export
data MultiConnection : {ms : ModuleSigsList} -> Modules ms -> Type where
  NoMC : MultiConnection End
  MkMC  : (subInps : List $ Fin $ totalInputs {ms} subMs) -> (topOuts : MFin m.outsCount) -> 
          (source : MFin $ allSrcsLen m ms subMs) -> (type : SVObject) -> 
          MultiConnection $ NewCompositeModule m {ms} subMs {sicons} {tocons} sssi ssto cont

namespace MultiConnectionsVect

  public export
  data MultiConnectionsVect : Nat -> Modules ms -> Type where
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
  data MSVObject : Type where
    Nothing : MSVObject
    Just    : SVObject -> MSVObject

  public export
  toVect : MultiConnectionsVect l n -> Vect l $ MultiConnection n
  toVect []      = []
  toVect (x::xs) = x :: toVect xs

  public export
  find : (mcs : MultiConnectionsVect l md) -> Fin l -> MSVObject
  find ((NoMC)           ::_ ) FZ     = Nothing
  find ((MkMC _ _ _ type)::_ ) FZ     = Just $ type
  find (_                ::fs) (FS i) = find fs i

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
resolveMultiConnections : {ms : ModuleSigsList} -> (m : Modules ms) -> (n : Nat ** MultiConnectionsVect n m)
resolveMultiConnections End                                                              = (0 ** [])
resolveMultiConnections md@(NewCompositeModule m subMs {sicons} {tocons} sssi ssto cont) = do
  let (_ ** rs) = resolveWithSource 
  let (_ ** rn) = resolveNoSource
  let rsl = toList rs
  let rnl = toList rn
  fromList $ rsl ++ rnl
  where

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
    
    resolveSource : Fin (allSrcsLen m ms subMs) -> Maybe $ MultiConnection md
    resolveSource f with (findSubSinks f) | (findTopSink f)
      resolveSource f | ss | ts with (m.inpsCount > finToNat f)
        resolveSource f | []         | Nothing    | False = Just $ MkMC [] Nothing    (Just f) $ unpOrGiven $ Just $ typeOf (allSrcs m ms subMs) f
        resolveSource f | []         | Nothing    | True  = Just $ MkMC [] Nothing    (Just f) $ typeOf (allSrcs m ms subMs) f
        resolveSource f | []         | (Just fts) | False = Just $ MkMC [] (Just fts) (Just f) $ typeOf (m.outputs) fts
        resolveSource f | []         | (Just fts) | True  = Nothing -- TopSink <- TopSource
        resolveSource f | ss@(x::xs) | Nothing    | False = Just $ MkMC ss Nothing    (Just f) $ unpOrGiven $ isUnpackedList ss
        resolveSource f | ss@(x::xs) | Nothing    | True  = Just $ MkMC ss Nothing    (Just f) $ typeOf (allSrcs m ms subMs) f
        resolveSource f | ss@(x::xs) | (Just fts) | False = Just $ MkMC ss (Just fts) (Just f) $ typeOf (m.outputs) fts
        resolveSource f | ss@(x::xs) | (Just fts) | True  = Nothing -- TopSink <- TopSource P.S. I hope such case is impossible

    resolveWithSource : (n : Nat ** MultiConnectionsVect n md)
    resolveWithSource = fromList $ catMaybes $ map resolveSource $ List.allFins $ (allSrcsLen m ms subMs) -- (\acc,x => resolveSource x :: acc)

    -- noSourceInps : (Fin $ totalInputs {ms} subMs, MFin $ allSrcsLen m ms subMs) -> Maybe $ MultiConnection md
    -- noSourceInps (fsk, Nothing) = Just $ MkMC [ fsk ] Nothing Nothing $ typeOf (allInputs {ms} subMs) fsk
    -- noSourceInps (_  , _      ) = Nothing

    -- noSourceOuts : (Fin m.outsCount, MFin src) -> Maybe $ MultiConnection md
    -- noSourceOuts (fsk, Nothing) = Just $ MkMC [] (Just fsk) Nothing $ typeOf m.outputs fsk
    -- noSourceOuts (_  , _      ) = Nothing

    -- routine : MFinsList a b -> ((Fin a, MFin b) -> Maybe $ MultiConnection md) -> List $ Maybe $ MultiConnection md
    -- routine mfin f = toList $ map f $ withIndex $ toVect mfin

    -- ||| Sink <- Nothing
    -- resolveNoSource : (n : Nat ** MultiConnectionsVect n md)
    -- resolveNoSource = fromList $ catMaybes $ (routine sicons noSourceInps) ++ (routine tocons noSourceOuts)

    noSourceInps : (Fin $ totalInputs {ms} subMs, MFin $ allSrcsLen m ms subMs) -> 
                   Maybe $ MultiConnection (NewCompositeModule m {ms} subMs {sicons} {tocons} sssi ssto cont)
    noSourceInps (fsk, Nothing) = Just $ MkMC [ fsk ] Nothing Nothing $ typeOf (allInputs {ms} subMs) fsk
    noSourceInps (_  , _      ) = Nothing

    noSourceOuts : (Fin m.outsCount, MFin src) -> Maybe $ MultiConnection (NewCompositeModule m {ms} subMs {sicons} {tocons} sssi ssto cont)
    noSourceOuts (fsk, Nothing) = Just $ MkMC [] (Just fsk) Nothing $ typeOf m.outputs fsk
    noSourceOuts (_  , _      ) = Nothing

    routine : MFinsList a b -> ((Fin a, MFin b) -> Maybe $ MultiConnection md) -> List $ Maybe $ MultiConnection md
    routine mfin f = toList $ map f $ withIndex $ toVect mfin

    ||| Sink <- Nothing
    resolveNoSource : (n : Nat ** MultiConnectionsVect n (NewCompositeModule m {ms} subMs {sicons} {tocons} sssi ssto cont))
    resolveNoSource = fromList $ catMaybes $ (routine sicons noSourceInps) ++ (routine tocons noSourceOuts)
