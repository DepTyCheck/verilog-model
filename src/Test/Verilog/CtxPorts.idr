module Test.Verilog.CtxPorts

import public Test.Verilog.SVType
import public Test.Verilog.Connections

import Data.Fin
import Data.Fin.Split -- indexSum

%default total

-- ||| Implicit cast situations:
-- |||
-- ||| module a(output logic [1:0] a1);
-- ||| endmodule: a
-- ||| 
-- ||| module b(input bit b1);
-- ||| endmodule: b
-- |||
-- ||| module c();
-- |||   a a_inst(w);      // #1  Cast logic [1:0] -> wire
-- |||   b b_inst(w);      // #1  Cast wire -> bit
-- ||| endmodule: c
-- |||
-- ||| module d(output byte d1, input reg d2);
-- |||   assign d1 = d2;   // #2  Cast reg -> byte
-- ||| endmodule: d
-- |||
-- ||| module e(input integer e1);
-- |||   b b_inst(e1);     // #3  Cast bit -> integer
-- ||| endmodule: e
-- |||
-- ||| module f(output int f1);
-- |||  a a_inst(f1);      // #4  Cast logic [1:0] -> int
-- ||| endmodule: f
-- |||
-- ||| module g();
-- |||   b b_inst(g2);     // #5  Cast wire -> bit
-- |||   a a_inst(g1);     // #6  Cast logic [1:0] -> wire
-- ||| endmodule: g
-- |||
-- ||| Implicit cast situations:                    | if unpacked then 0 casts else ...
-- ||| 1. Submodule source -> submodule sink        | 2 casts (source type -> default_net_type, default_net_type -> sink type)
-- ||| 2. Top source -> top sink                    | 1 cast  (source type -> sink type)
-- ||| 3. Top source -> submodule sink              | 1 cast  (source type -> sink type)
-- ||| 4. Submodule source -> top sink              | 1 cast  (source type -> sink type)
-- ||| 5. Unconnected submodule sink                | 1 cast  (source type -> default_net_type)
-- ||| 6. Unconnected submodule source              | 1 cast  (default_net_type -> sink type)  -- calculated in Pretty

public export
fixSSFin : (m : ModuleSig) -> (ms : ModuleSigsList) -> (subMs : FinsList ms.length) -> Fin (totalOutputs {ms} subMs) ->
            Fin $ allSrcsLen m ms subMs
fixSSFin m ms subMs f = comFin $ shift m.inpsCount f

toTotalInputsIdx' : {ms : _} -> {subMs : FinsList ms.length} ->
                    (idx : Fin subMs.length) ->
                    Fin (index ms (index subMs idx)).inpsCount ->
                    Fin $ totalInputs {ms} subMs
toTotalInputsIdx' {subMs=i::is} idx x with 0 (sym $ svolistAppendLen (index ms i).inputs (allInputs {ms} is))
                                          | 0 (length ((index ms i).inputs ++ allInputs {ms} is))
  toTotalInputsIdx' FZ       x | Refl | _ = indexSum $ Left x
  toTotalInputsIdx' (FS idx) x | Refl | _ = indexSum $ Right $ toTotalInputsIdx' idx x

toTotalOutputsIdx' : {ms : _} -> {subMs : FinsList ms.length} ->
                     (idx : Fin subMs.length) ->
                     Fin (index ms (index subMs idx)).outsCount ->
                     Fin $ totalOutputs {ms} subMs
toTotalOutputsIdx' {subMs=i::is} idx x with 0 (sym $ svolistAppendLen (index ms i).outputs (allOutputs {ms} is))
                                          | 0 (length ((index ms i).outputs ++ allOutputs {ms} is))
  toTotalOutputsIdx' FZ       x | Refl | _ = indexSum $ Left x
  toTotalOutputsIdx' (FS idx) x | Refl | _ = indexSum $ Right $ toTotalOutputsIdx' idx x

inpsTotalFins : (ms : ModuleSigsList) -> (subMs : FinsList ms.length) ->
                (Fin $ subMs.length) -> List (Fin $ totalInputs {ms} subMs)
inpsTotalFins ms subMs subMsIdx = List.allFins (index ms $ index subMs subMsIdx).inpsCount <&> toTotalInputsIdx' subMsIdx

inpsTotalFins' : (ms : ModuleSigsList) -> (subMs : FinsList ms.length) ->
                 (Fin $ subMs.length) -> FinsList (totalInputs {ms} subMs)
inpsTotalFins' ms subMs subMsIdx = fromList $ inpsTotalFins ms subMs subMsIdx

outsTotalFins : (ms : ModuleSigsList) -> (subMs : FinsList ms.length) ->
                (Fin $ subMs.length) -> List (Fin $ totalOutputs {ms} subMs)
outsTotalFins ms subMs subMsIdx = List.allFins (index ms $ index subMs subMsIdx).outsCount <&> toTotalOutputsIdx' subMsIdx

outsTotalFins' : (ms : ModuleSigsList) -> (subMs : FinsList ms.length) ->
                 (Fin $ subMs.length) -> FinsList (totalOutputs {ms} subMs)
outsTotalFins' ms subMs subMsIdx = fromList $ outsTotalFins ms subMs subMsIdx

tryFindTopPort : (d : SVObjList) ->  MFinsList l (d.length) -> Fin (d.length) -> Maybe SVObject
tryFindTopPort _ []               f = Nothing
tryFindTopPort d (Nothing  :: xs) f = tryFindTopPort d xs f
tryFindTopPort d ((Just x) :: xs) f = if x == f then Just (typeOf d f) else tryFindTopPort d xs f

finInMFL : MFinsList l n -> Fin n -> Bool
finInMFL []               f = False
finInMFL (Nothing  :: xs) f = finInMFL xs f
finInMFL ((Just x) :: xs) f = if x == f then True else finInMFL xs f

export
resolveLocalCtxPortTypes : {ms : _} -> Modules ms -> ModuleSigsList
resolveLocalCtxPortTypes End                                                       = []
resolveLocalCtxPortTypes {ms} (NewCompositeModule m subMs {sicons} {tocons} _ _ _) = 
  reverse $ foldl (\acc,x => resolve' x :: acc) [] $ List.allFins subMs.length where

  sources : SVObjList
  sources = allSrcs m ms subMs

  resolveSink : Fin (totalInputs {ms} subMs) -> SVObject
  resolveSink subInp = do 
    let outerCtxType = typeOf (allInputs {ms} subMs) subInp
    case isUnpacked outerCtxType of
      True => outerCtxType -- The port is unpacked and thus explicitly declared
      False => do
        let srcIdxRaw = find sicons subInp -- Find the source (top input port) index
        case srcIdxRaw of
          Nothing     => defaultNetType -- Sink is unconnected
          Just srcIdx => if m.inpsCount > (finToNat srcIdx)
            then typeOf sources srcIdx -- Sink is connected to top source
            else case tryFindTopPort sources tocons srcIdx of
              Nothing => defaultNetType   -- Sink is connected to the source which IS NOT connected to top output
              (Just svobject) => svobject -- Sink is connected to the source which IS connected to top output

  resolveSource : Fin (length $ allOutputs {ms} subMs) -> SVObject
  resolveSource subOut with (isUnpacked $ typeOf (allOutputs {ms} subMs) subOut)
    resolveSource subOut | True  = typeOf (allOutputs {ms} subMs) subOut -- The port is unpacked and thus explicitly declared
    resolveSource subOut | False with (finInMFL tocons $ fixSSFin m ms subMs subOut)
      resolveSource subOut | False | True = case tryFindTopPort sources tocons (fixSSFin m ms subMs subOut) of -- Source is connected to top output
        Nothing => defaultNetType -- IMPOSSIBLE CASE. REFACTOR
        (Just topOutType) => topOutType
      resolveSource subOut | False | False = defaultNetType -- Source is NOT connected to any sink OR connected to a submodule input

  resolveSinks : (fins : FinsList $ totalInputs {ms} subMs) -> SVObjList
  resolveSinks fins = reverse $ foldl (\acc,x => resolveSink x :: acc) [] $ fins.asList

  resolveSources : (fins : FinsList $ totalOutputs {ms} subMs) -> SVObjList
  resolveSources fins = reverse $ foldl (\acc,x => resolveSource x :: acc) [] $ fins.asList
  
  resolve' : Fin (subMs.length) -> ModuleSig
  resolve' f = MkModuleSig (resolveSinks $ inpsTotalFins' ms subMs f) (resolveSources $ outsTotalFins' ms subMs f)
