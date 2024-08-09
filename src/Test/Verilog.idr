module Test.Verilog

import Data.Fuel
import public Data.Fin

import Test.DepTyCheck.Gen

%default total

namespace ModuleSig

  public export
  record ModuleSig where
    constructor MkModuleSig
    inputs  : Nat
    outputs : Nat

  %name ModuleSig m

namespace FinsList

  public export
  data FinsList : Nat -> Type where
    Nil  : FinsList n
    (::) : Fin n -> FinsList n -> FinsList n

  %name FinsList fs

  public export
  (.asList) : FinsList n -> List (Fin n)
  (.asList) []      = []
  (.asList) (x::xs) = x :: xs.asList

  public export
  (.length) : FinsList n -> Nat
  (.length) [] = 0
  (.length) (x::xs) = S xs.length

  public export
  (++) : (xs, ys : FinsList n) -> FinsList n
  [] ++ ys = ys
  (x :: xs) ++ ys = x :: xs ++ ys

  public export
  index : (fs : FinsList n) -> Fin (fs.length) -> Fin n
  index (y :: fs) FZ = y
  index (y :: fs) (FS x) = index fs x

namespace Connections
  public export
  data Connections : (ins, outs : Nat) -> Type where
    Nil  : Connections ints Z
    (::) : Fin ins -> Connections ins outs -> Connections ins (S outs)

  public export
  connSplit : {a : Nat} -> Connections f (a + b) -> (Connections f a, Connections f b)
  connSplit {a=0} x = ([], x)
  connSplit {a=(S k)} (x :: y) with (connSplit {a=k} y)
    connSplit {a=(S k)} (x :: y) | (xs, ys) = (x :: xs, ys)

  public export
  (++) : Connections f a -> Connections f b -> Connections f (a + b)
  (++) [] y = y
  (++) (x :: z) y = x :: (++) z y

  public export
  index : Connections f l -> Fin l -> Fin f
  index (x::xs) FZ = x
  index (x::xs) (FS y) = index xs y

  public export
  finMap : (Fin a -> Fin b) -> Connections a l -> Connections b l
  finMap f [] = []
  finMap f (x::xs) = f x :: finMap f xs

public export
data ContextModuleList : Type

%name ContextModuleList ms

public export
data ContextModule : (ms: ContextModuleList) -> Type

public export
cmSig : ContextModule ms -> ModuleSig

public export
data ContextModuleList : Type where
  Nil : ContextModuleList
  Cons : (ms : ContextModuleList) -> (ContextModule ms) -> ContextModuleList

public export
length : ContextModuleList -> Nat
length []      = Z
length (Cons ms _) = S $ length ms

public export %inline
(.length) : ContextModuleList -> Nat
(.length) = length

public export
indexSig : (ms: ContextModuleList) -> (idx : Fin ms.length) -> ModuleSig
indexSig (Cons _ x) FZ = cmSig x
indexSig (Cons ms _) (FS i) = indexSig ms i

public export
indexTail : (ms : ContextModuleList) -> (idx : Fin ms.length) -> ContextModuleList
indexTail (Cons ms x) FZ = ms
indexTail (Cons ms x) (FS y) = indexTail ms y

public export
indexFlawed : (ms : ContextModuleList) -> (idx : Fin ms.length) -> ContextModule $ indexTail ms idx
indexFlawed (Cons ms x) FZ = x
indexFlawed (Cons ms x) (FS y) = indexFlawed ms y

public export
totalInputs : {ms : ContextModuleList} -> FinsList ms.length -> Nat
totalInputs []      = 0
totalInputs (i::is) = (indexSig ms i).inputs + totalInputs {ms} is

public export
totalOutputs : {ms : ContextModuleList} -> FinsList ms.length -> Nat
totalOutputs []      = 0
totalOutputs (i::is) = (indexSig ms i).outputs + totalOutputs {ms} is

public export
SubmoduleList : ContextModuleList -> Type
SubmoduleList ms = FinsList $ ms.length

public export
ConnectionsOf : {ms: ContextModuleList} -> ModuleSig -> SubmoduleList ms -> Type
ConnectionsOf m subMs = Connections (m.inputs + totalOutputs {ms} subMs) (m.outputs + totalInputs {ms} subMs)

public export
record Module (ms: ContextModuleList) where
  constructor MModule
  sig : ModuleSig
  subMs : SubmoduleList ms
  conns : ConnectionsOf {ms} sig subMs



public export
data ContextModule : (ms: ContextModuleList) -> Type where
  SignatureOnly : (m : ModuleSig) -> ContextModule ms
  FullModule : (mod : Module ms) -> ContextModule ms

cmSig (SignatureOnly m) = m
cmSig (FullModule (MModule m _ _)) = m

public export
data ModuleList : (ms : ContextModuleList) -> Type where
  End : ModuleList ms
  MCons : (mod : Module ms) -> ModuleList (Cons ms (FullModule mod)) -> ModuleList ms

export
genModuleList : Fuel -> (ms : ContextModuleList) -> Gen MaybeEmpty $ ModuleList ms
