module Test.Verilog.Inline

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


-- Submodules for ms to submodules for (m::ms)
promoteSubMsByOne : FinsList k -> FinsList (S k)
promoteSubMsByOne [] = []
promoteSubMsByOne (x :: fs) = FS x :: promoteSubMsByOne fs

-- Lemma: totalOutputs (promoteSubMsByOne subMs) = totalOutputs subMs
promoteTotalOutputs: {ms : ContextModuleList} -> {c : ContextModule ms} -> (subMs : SubmoduleList ms) ->
      (totalOutputs {ms=Cons ms c} (promoteSubMsByOne subMs)) = (totalOutputs {ms} subMs)
promoteTotalOutputs [] = Refl
promoteTotalOutputs (x :: fs) = rewrite promoteTotalOutputs {ms} {c} fs in Refl

-- Lemma: totalInputs (promoteSubMsByOne subMs) = totalInputs subMs
promoteTotalInputs : {ms : ContextModuleList} -> {c : ContextModule ms} -> (subMs : SubmoduleList ms) ->
        (totalInputs {ms=Cons ms c} (promoteSubMsByOne subMs)) = (totalInputs {ms} subMs)
promoteTotalInputs [] = Refl
promoteTotalInputs (x :: fs) = rewrite promoteTotalInputs {ms} {c} fs in Refl

-- Connections for ms to connections for (m::ms)
promoteConnsByOne :
  {ms : ContextModuleList} -> {c : ContextModule ms} -> (subMs : SubmoduleList ms) -> (m : ModuleSig) ->
  ConnectionsOf {ms} m subMs -> ConnectionsOf {ms=Cons ms c} m (promoteSubMsByOne subMs)
promoteConnsByOne [] m x = x
promoteConnsByOne (y :: fs) m x = rewrite promoteTotalInputs {ms} {c} fs in rewrite promoteTotalOutputs {ms} {c} fs in x

-- Module for ms to module for (m::ms)
promoteByOne : {ms : ContextModuleList} -> {c : ContextModule ms} -> Module ms -> Module (Cons ms c)
promoteByOne (MModule m subMs x) = MModule m (promoteSubMsByOne subMs) (promoteConnsByOne subMs m x)

-- Lemma: promoteByOne doesn't change the signature
promoteByOneSigEq :
  {ms : ContextModuleList} -> {c : ContextModule ms} -> (mod: Module ms) ->
  (mod.sig = (promoteByOne {ms} {c} mod).sig)
promoteByOneSigEq (MModule m _ _) = Refl

-- Convert module acquired by (index ms idx) to depend on ms as opposed to (indexTail ms idx)
promoteIndexed : (ms : ContextModuleList) -> (idx : Fin ms.length) -> Module (indexTail ms idx) -> Module ms
promoteIndexed (Cons ms c) FZ x = promoteByOne x
promoteIndexed (Cons ms c) (FS z) x = promoteByOne $ promoteIndexed ms z x

-- Lemma: promoteIndexed doesn't change the module signature
promoteIndexedSigEq :
  (ms : ContextModuleList) -> (idx : Fin ms.length) -> (mod: Module (indexTail ms idx)) ->
  (mod.sig = (promoteIndexed ms idx mod).sig)
promoteIndexedSigEq (Cons ms c) FZ mod = promoteByOneSigEq {c} mod
promoteIndexedSigEq (Cons ms c) (FS y) mod =
  rewrite promoteIndexedSigEq ms y mod in promoteByOneSigEq {c} $ promoteIndexed ms y mod

-- Lemma: totalInputs (a ++ b) = totalInputs a ++ totalInputs b
totalInputsDistributive :
  (ms : ContextModuleList) -> (subMs : SubmoduleList ms) -> (subMs' : SubmoduleList ms) ->
  totalInputs {ms} (subMs ++ subMs') = totalInputs {ms} subMs + totalInputs {ms} subMs'
totalInputsDistributive ms [] subMs' = Refl
totalInputsDistributive ms (z :: fs) subMs' =
  rewrite totalInputsDistributive ms fs subMs' in plusAssociative (indexSig ms z).inputs (totalInputs fs) (totalInputs subMs')

-- Lemma: totalOutputs (a ++ b) = totalOutputs a ++ totalOutputs b
totalOutputsDistributive :
  (ms : ContextModuleList) -> (subMs : SubmoduleList ms) -> (subMs' : SubmoduleList ms) ->
  totalOutputs {ms} (subMs ++ subMs') = totalOutputs {ms} subMs + totalOutputs {ms} subMs'
totalOutputsDistributive ms [] subMs' = Refl
totalOutputsDistributive ms (z :: fs) subMs' =
  rewrite totalOutputsDistributive ms fs subMs' in plusAssociative (indexSig ms z).outputs (totalOutputs fs) (totalOutputs subMs')

weakenConns : {n : Nat} -> Connections m l -> Connections (m + n) l
weakenConns = finMap $ weakenN n

-- Cut out the connections of a nth submodule
connsOfSubM : (ms : ContextModuleList) -> (subMs : SubmoduleList ms) -> (n : Fin subMs.length) ->
  Connections f (totalInputs {ms} subMs) -> Connections f (indexSig ms (index subMs n)).inputs
connsOfSubM ms (y :: fs) FZ x = fst $ connSplit x
connsOfSubM ms (y :: fs) (FS z) x = connsOfSubM ms fs z $ snd $ connSplit x

-- Promote a single connection for m' and subMs' into one for m and (subMs + subMs'),
-- given a mapping of toplevel outputs for m'
integrateConn :
  {ms : ContextModuleList} ->
  (m : ModuleSig) -> (subMs : SubmoduleList ms) -> (conns : ConnectionsOf {ms} m subMs) ->
  (m' : ModuleSig) -> (subMs' : SubmoduleList ms) ->
  Connections (m.inputs + totalOutputs {ms} subMs) m'.inputs ->
  Fin (m'.inputs + totalOutputs {ms} subMs') ->
  Fin (m.inputs + (totalOutputs {ms} subMs + totalOutputs {ms} subMs'))
integrateConn m subMs conns m' subMs' mapping f with (splitSum f)
  integrateConn m subMs conns m' subMs' mapping f | Left ll = replace {p=Fin} rule $ weakenN (totalOutputs subMs') $ index mapping ll
    where rule = sym $ plusAssociative (m.inputs) (totalOutputs subMs) (totalOutputs subMs')
  integrateConn m subMs conns m' subMs' mapping f | Right rr = shift m.inputs . shift (totalOutputs {ms} subMs) $ rr

-- Concatenate ConnectionsOf m subMs and ConnectionsOf m' subMs' into ConnectionsOf m' (subMs ++ subMs'),
-- given a mapping of toplevel outputs for m'
concatConns :
  {ms : ContextModuleList} ->
  (m : ModuleSig) -> (subMs : SubmoduleList ms) -> (conns : ConnectionsOf {ms} m subMs) ->
  (m' : ModuleSig) -> (subMs' : SubmoduleList ms) -> (conns' : ConnectionsOf {ms} m' subMs') ->
  Connections (m.inputs + totalOutputs {ms} subMs) m'.inputs ->
  ConnectionsOf {ms} m (subMs ++ subMs')
concatConns m subMs conns m' subMs' conns' mapping =
  rewrite totalInputsDistributive ms subMs subMs' in
  rewrite totalOutputsDistributive ms subMs subMs' in do
    let (connOutputs, connSubs) = connSplit {a=m.outputs} $ weakenConns {n=totalOutputs subMs'} $ conns

    let rule = plusAssociative (m.inputs) (totalOutputs {ms} subMs) (totalOutputs {ms} subMs')

    let (subOutputs, subSubs) = connSplit {a=m'.outputs} conns'

    let newSubSubs = flip finMap subSubs $ integrateConn {ms} m subMs conns m' subMs' mapping

    (rewrite rule in connOutputs) ++ (rewrite rule in connSubs) ++ newSubSubs

--
indexWithPromotion : (ms : ContextModuleList) -> (idx : Fin ms.length) -> Maybe (mod : Module ms ** mod.sig = indexSig ms idx)
indexWithPromotion ms idx = do
  (mod' ** indexSigEq) <- indexFlawed' ms idx

  let promotedSigEq = promoteIndexedSigEq ms idx mod'
  Just (promoteIndexed ms idx mod' ** trans (sym promotedSigEq) indexSigEq)
  where
    indexFlawed' : (ms : ContextModuleList) -> (idx : Fin ms.length) -> Maybe (mod : Module (indexTail ms idx) ** mod.sig = indexSig ms idx)
    indexFlawed' (Cons ms (FullModule (MModule sig subMs conns))) FZ = Just ((MModule sig subMs conns) ** Refl)
    indexFlawed' (Cons ms (SignatureOnly _)) FZ = Nothing
    indexFlawed' (Cons ms x) (FS y) = indexFlawed' ms y

subMsBefore : {ms : ContextModuleList} -> (subMs : SubmoduleList ms) -> (idx : Fin subMs.length) -> SubmoduleList ms
subMsBefore (x :: fs) FZ = []
subMsBefore (x :: fs) (FS y) = x :: subMsBefore fs y

subMsAfter : {ms : ContextModuleList} -> (subMs : SubmoduleList ms) -> (idx : Fin subMs.length) -> SubmoduleList ms
subMsAfter (x :: fs) FZ = fs
subMsAfter (x :: fs) (FS y) = subMsAfter fs y

emap : {a : Type} -> {b : Type} -> {x : a} -> {y : a} -> (f : a -> b) -> x=y -> (f x) = (f y)
emap f Refl = Refl

subMsThreeWaySplit :
  {ms : ContextModuleList} -> (subMs : SubmoduleList ms) -> (idx : Fin subMs.length) ->
  subMs = (subMsBefore {ms} subMs idx) ++ [index subMs idx] ++ (subMsAfter {ms} subMs idx)
subMsThreeWaySplit (x :: fs) FZ = Refl
subMsThreeWaySplit (x :: fs) (FS y) = do
  emap (x::) $ subMsThreeWaySplit fs y

checkOneConn : {ms : ContextModuleList} -> Fuel -> (m : ModuleSig) ->
  (subMs : SubmoduleList ms) -> (idx : Fin subMs.length) ->
  (subMs' : SubmoduleList ms) ->
  (mapping : Connections (m.inputs + (totalOutputs {ms} (subMs ++ subMs'))) (indexSig ms (index subMs idx)).outputs) ->
  Fin (m.inputs + (totalOutputs {ms} (subMs ++ subMs'))) ->
  Maybe $ Fin (m.inputs + totalOutputs {ms} (subMsBefore {ms} subMs idx ++ subMsAfter {ms} subMs idx ++ subMs'))
checkOneConn Dry m subMs idx subMs' mapping x = Nothing
checkOneConn (More fuel) m subMs idx subMs' mapping x with (splitSum x)
  checkOneConn (More fuel) m subMs idx subMs' mapping x | Left y = Just $ weakenN (totalOutputs {ms} (subMsBefore {ms} subMs idx ++ subMsAfter {ms} subMs idx ++ subMs')) y
  checkOneConn (More fuel) m subMs idx subMs' mapping x | Right y with (splitSum (replace {p=Fin} (totalOutputsDistributive ms subMs subMs') y))
    checkOneConn (More fuel) m subMs idx subMs' mapping x | Right y | Left z = do
      let splitRule = subMsThreeWaySplit {ms} subMs idx
      let z = replace {p=Fin . totalOutputs} splitRule z
      let distRule0 = totalOutputsDistributive ms (subMsBefore {ms} subMs idx) (index subMs idx :: subMsAfter {ms} subMs idx)
      let z = replace {p=Fin} distRule0 z
      case splitSum z of
        Left zz => do
          let distRule0 = sym $ totalOutputsDistributive ms (subMsBefore {ms} subMs idx) (subMsAfter {ms} subMs idx ++ subMs')
          Just $ shift m.inputs $ replace {p=Fin} distRule0 $ weakenN (totalOutputs {ms} (subMsAfter {ms} subMs idx ++ subMs')) zz
        Right zz => case splitSum zz of
          Left zzz => do
            checkOneConn fuel m subMs idx subMs' mapping $ index mapping zzz
          Right zzz => do
            let distRule0 = sym $ totalOutputsDistributive ms (subMsBefore {ms} subMs idx) (subMsAfter {ms} subMs idx ++ subMs')
            let distRule1 = sym $ totalOutputsDistributive ms (subMsAfter {ms} subMs idx) subMs'
            Just $ shift m.inputs $
              replace {p=Fin} distRule0 $ shift (totalOutputs {ms} (subMsBefore {ms} subMs idx)) $
              replace {p=Fin} distRule1 $ weakenN (totalOutputs {ms} subMs') zzz
    checkOneConn (More fuel) m subMs idx subMs' mapping x | Right y | Right z = do
      let distRule0 = sym $ totalOutputsDistributive ms (subMsBefore {ms} subMs idx) (subMsAfter {ms} subMs idx ++ subMs')
      let distRule1 = sym $ totalOutputsDistributive ms (subMsAfter {ms} subMs idx) subMs'
      Just $ shift m.inputs $
        replace {p=Fin} distRule0 $ shift (totalOutputs {ms} (subMsBefore {ms} subMs idx)) $
        replace {p=Fin} distRule1 $ shift (totalOutputs {ms} (subMsAfter {ms} subMs idx)) z


checkAllConns : {ms : ContextModuleList} -> Fuel -> (m : ModuleSig) ->
  (subMs : SubmoduleList ms) -> (idx : Fin subMs.length) ->
  (subMs' : SubmoduleList ms) ->
  (mapping : Connections (m.inputs + (totalOutputs {ms} (subMs ++ subMs'))) (indexSig ms (index subMs idx)).outputs) ->
  Connections (m.inputs + (totalOutputs {ms} (subMs ++ subMs'))) l ->
  Maybe $ Connections (m.inputs + totalOutputs {ms} (subMsBefore {ms} subMs idx ++ subMsAfter {ms} subMs idx ++ subMs')) l
checkAllConns fuel m subMs idx subMs' mapping [] = Just []
checkAllConns fuel m subMs idx subMs' mapping (x :: xs) = do
  x' <- checkOneConn {ms} fuel m subMs idx subMs' mapping x
  xs' <- checkAllConns fuel m subMs idx subMs' mapping xs
  Just $ x' :: xs'

-- Eureka moment, try implementing???
-- Also, need to filter for graph cycles
connRerouteNew : {ms : ContextModuleList} -> (m : ModuleSig) ->
  (subMs : SubmoduleList ms) -> (idx : Fin subMs.length) ->
  (subMs' : SubmoduleList ms) ->
  Connections (m.inputs + totalOutputs {ms} (subMsBefore {ms} subMs idx ++ subMsAfter {ms} subMs idx ++ subMs')) (indexSig ms (index subMs idx)).outputs ->
  Fin (m.inputs + (totalOutputs {ms} (subMs ++ subMs'))) ->
  Fin (m.inputs + totalOutputs {ms} (subMsBefore {ms} subMs idx ++ subMsAfter {ms} subMs idx ++ subMs'))
connRerouteNew m subMs idx subMs' mapping x with (splitSum x)
  connRerouteNew m subMs idx subMs' mapping x | Left ll = weakenN (totalOutputs (subMsBefore subMs idx ++ (subMsAfter subMs idx ++ subMs'))) ll
  connRerouteNew m subMs idx subMs' mapping x | Right rr = do
    let splitRule = subMsThreeWaySplit subMs idx
    let rr = replace {p=(\x=>Fin (totalOutputs {ms} (x ++ subMs')))} splitRule rr
    -- Fin $ totalOutputs $ (subMsBefore subMs idx ++ (index subMs idx :: subMsAfter subMs idx)) ++ subMs'
    let dist1 = totalOutputsDistributive ms (subMsBefore {ms} subMs idx ++ (index subMs idx :: subMsAfter {ms} subMs idx)) subMs'
    let rr = replace {p=Fin} dist1 rr
    -- rr : Fin $ plus (totalOutputs (subMsBefore subMs idx ++ (index subMs idx :: subMsAfter subMs idx))) (totalOutputs subMs')
    let dist2 = totalOutputsDistributive ms (subMsBefore {ms} subMs idx) (index subMs idx :: subMsAfter {ms} subMs idx)
    let rr = replace {p=(\x=>Fin(x + (totalOutputs subMs')))} dist2 rr

    -- Fin (totalOutputs (subMsBefore subMs idx ++ (subMsAfter subMs idx ++ subMs')))
    let dist3 = sym $ totalOutputsDistributive ms (subMsBefore {ms} subMs idx) (subMsAfter subMs idx ++ subMs')
    -- Fin (plus (totalOutputs (subMsBefore subMs idx)) (totalOutputs (subMsAfter subMs idx ++ subMs')))
    let dist4 = sym $  totalOutputsDistributive ms (subMsAfter subMs idx) subMs'

    case splitSum rr of
      Left lll => case splitSum lll of
        Left lll => do
          shift m.inputs $
            replace {p=Fin} dist3 $
            replace {p=(\x=>Fin (totalOutputs {ms} (subMsBefore {ms} subMs idx) + x))} dist4 $
            weakenN (plus (totalOutputs {ms} (subMsAfter {ms} subMs idx)) (totalOutputs {ms} subMs')) lll
        Right rrr => case splitSum rrr of
          Left lll => index mapping lll
          Right rrr => shift m.inputs $
            replace {p=Fin} dist3 $
            replace {p=(\x=>Fin (totalOutputs {ms} (subMsBefore {ms} subMs idx) + x))} dist4 $
            shift (totalOutputs {ms} (subMsBefore {ms} subMs idx)) $
            weakenN (totalOutputs {ms} subMs') rrr
      Right rrr => shift m.inputs $
            replace {p=Fin} dist3 $
            replace {p=(\x=>Fin (totalOutputs {ms} (subMsBefore {ms} subMs idx) + x))} dist4 $
            shift (totalOutputs {ms} (subMsBefore {ms} subMs idx)) $
            shift (totalOutputs {ms} (subMsAfter {ms} subMs idx)) $
            rrr

connsExciseNew :
  {ms : ContextModuleList} ->
  (m : ModuleSig) -> (subMs : SubmoduleList ms) -> (idx : Fin subMs.length) ->
  (subMs' : SubmoduleList ms) ->
  Connections n (m.outputs + totalInputs {ms} (subMs ++ subMs')) ->
  Connections n (m.outputs + totalInputs {ms} (subMsBefore {ms} subMs idx ++ (subMsAfter {ms} subMs idx ++ subMs')))
connsExciseNew m subMs idx subMs' x = do
  let (a, b) = connSplit x
  let distRule = totalInputsDistributive ms subMs subMs'
  let b = replace {p=(Connections n)} distRule b
  let (c, d) = connSplit b
  let splitRule = subMsThreeWaySplit {ms} subMs idx
  let c = replace {p=(Connections n) . (totalInputs {ms})} splitRule c
  let dr0 = totalInputsDistributive ms (subMsBefore subMs idx) (index subMs idx :: subMsAfter subMs idx)
  let c = replace {p=(Connections n)} dr0 c
  let (e, f) = connSplit c
  let (g, h) = connSplit f
  let dr1 = sym $ totalInputsDistributive ms (subMsAfter subMs idx) subMs'
  let i = replace {p=(Connections n)} dr1 $ h ++ d
  let dr2 = sym $ totalInputsDistributive ms (subMsBefore subMs idx) (subMsAfter subMs idx ++ subMs')
  let j = replace {p=(Connections n)} dr2 $ e ++ i
  a ++ j

public export
inlineNthSubModule : {ms : ContextModuleList} -> Fuel -> (mod : Module ms) -> (idx : Fin mod.subMs.length) -> Maybe $ Module ms
inlineNthSubModule fuel (MModule m subMs conns) idx = do
  ((MModule m' subMs' conns') ** prf) <- indexWithPromotion ms (index subMs idx)
  let (connsOut, connsSub) = connSplit {a=m.outputs} conns
  let (submoduleOut, _) = connSplit {a=m'.outputs} conns'


  let mapping : Connections (m .inputs + totalOutputs subMs) (m' .inputs) =
    rewrite prf in connsOfSubM ms subMs idx connsSub
  let concatConns = concatConns m subMs conns m' subMs' conns' mapping

  let toRule = sym $ totalOutputsDistributive ms subMs subMs'

  let integr = integrateConn m subMs conns m' subMs' mapping
  let smoIntegrated = finMap integr submoduleOut
  let smoIntegrated = replace {p=(\x=>Connections (m.inputs + x) m'.outputs)} toRule smoIntegrated
  let smoIntegratedMapping = replace {p=(\x=>Connections (plus (m .inputs) (totalOutputs (subMs ++ subMs'))) x.outputs)} prf smoIntegrated

  checked <- checkAllConns fuel m subMs idx subMs' smoIntegratedMapping smoIntegrated


  let connsReroute = finMap $ connRerouteNew {ms} m subMs idx subMs' (rewrite sym prf in checked)

  let finalConns = connsExciseNew {ms} m subMs idx subMs' . connsReroute $ concatConns

  Just $ MModule m (subMsBefore {ms} subMs idx ++ (subMsAfter {ms} subMs idx ++ subMs')) finalConns
