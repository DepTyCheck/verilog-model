module Test.Verilog.Assign

import public Test.Verilog.Module

import Data.Fuel
import Data.Vect
import Data.Vect.Extra

import Test.DepTyCheck.Gen

-- ILLEGAL: Top inputs, submodule outputs

||| Could be several assings if type could be multi driven
public export
multidriven : SVBasic -> Bool
multidriven Logic'   = False
multidriven Wire'    = True
multidriven Uwire'   = False
multidriven Int'     = False
multidriven Integer' = False
multidriven Bit'     = False
multidriven Real'    = False

public export
multidriven' : SVType -> Bool
multidriven' (Var x)                    = multidriven x
multidriven' (Arr (Packed   t {} @{_})) = multidriven' t
multidriven' (Arr (Unpacked t {}))      = multidriven' t

||| Unconnected sumbodule inputs + unconnected top outputs
-- public export
-- portsToAssign : (inps: Nat) -> Vect sk (Maybe $ Fin ss) -> List (Fin sk)
-- portsToAssign inps v = do
--   let (_ ** res) = catMaybes $ map resolve' $ withIndex v -- where
--   toList res where
--     resolve': (Fin sk, Maybe (Fin ss)) -> Maybe (Fin sk)
--     resolve' (x, Nothing) = Just x
--     resolve' (x, (Just y)) = Nothing

-- public export
-- data Assign : PortsList -> PortsList -> Type where
--   Existing : (Fin sk.length) -> Assign sk sk
--   New      : (t: SVType)     -> Assign sk (t::sk)

-- public export
-- data AssignsList : PortsList -> PortsList -> Type
-- public export
-- data CanAssign   : (sk: PortsList) -> AssignsList sk ska -> Assign sk ska -> Type

-- public export
-- data AssignsList : PortsList -> PortsList -> Type where
--   Empty  : AssignsList [] _
--   Cons   : (a: Assign sk ska) -> (pre: AssignsList sk ska) -> CanAssign sk pre a -> AssignsList sk ska

-- public export
-- data EqBool : Bool -> Bool -> Type where
--   Same : (x : Bool) -> EqBool x x

-- public export
-- data FinNotIn : (FinsList n) -> (Fin n) -> Type where
--   FNIEmpty : FinNotIn [] f
--   FNICons  : {x, f: Fin n} -> (0 _ : So $ x /= f) -> (npi: FinNotIn xs f) -> FinNotIn (x :: xs) f

-- public export
-- toFinsList : AssignsList sk ska -> FinsList sk.length
-- toFinsList Empty = []
-- toFinsList (Cons a pre _) = case a of
--   Existing fin => fin :: toFinsList pre
--   New _        => toFinsList pre

-- public export
-- data CanAssign : (sk: PortsList) -> AssignsList sk ska -> Assign sk ska -> Type where
--   ExSignle : EqBool False (multidriven' $ typeOf sk finPorts) -> FinNotIn (toFinsList assigns) finPorts -> CanAssign sk assigns (Existing finPorts)
--   ExMulti  : EqBool True  (multidriven' $ typeOf sk finPorts) -> CanAssign sk assigns (Existing finPorts)
--   NewAny   : CanAssign sk assigns (New t)

-- export
-- genAssigns : Fuel -> (sk: PortsList) -> Gen MaybeEmpty $ AssignsList sk ska

public export
data Assign : PortsList -> Type where
  Existing : (Fin sk.length) -> Assign sk
  New      : SVType          -> Assign sk

public export
data AssignsList : PortsList -> Type
public export
data CanAssign   : (sk: PortsList) -> AssignsList sk -> Assign sk -> Type

public export
data AssignsList : (sk: PortsList) -> Type where
  Empty  : AssignsList []
  Cons   : (a: Assign sk) -> (pre: AssignsList sk) -> CanAssign sk pre a -> AssignsList sk

public export
data EqBool : Bool -> Bool -> Type where
  Same : (x : Bool) -> EqBool x x

public export
data FinNotIn : (FinsList n) -> (Fin n) -> Type where
  FNIEmpty : FinNotIn [] f
  FNICons  : {x, f: Fin n} -> (0 _ : So $ x /= f) -> (npi: FinNotIn xs f) -> FinNotIn (x :: xs) f

public export
toFinsList : AssignsList sk -> FinsList sk.length
toFinsList Empty = []
toFinsList (Cons a pre _) = case a of
  Existing fin => fin :: toFinsList pre
  New _        => toFinsList pre

public export
data CanAssign : (sk: PortsList) -> AssignsList sk -> Assign sk -> Type where
  ExSignle : EqBool False (multidriven' $ typeOf sk finPorts) -> FinNotIn (toFinsList assigns) finPorts -> CanAssign sk assigns (Existing finPorts)
  ExMulti  : EqBool True  (multidriven' $ typeOf sk finPorts) -> CanAssign sk assigns (Existing finPorts)
  NewAny   : CanAssign sk assigns (New _)

export
genAssigns : Fuel -> (sk: PortsList) -> Gen MaybeEmpty $ AssignsList sk



















-- public export
-- data Assign : (FinsList sk) -> Type where
--   Existing : (Fin sk.length) -> Assign sk
--   New      : SVType          -> Assign sk

-- public export
-- data AssignsList : (sk: PortsList) -> (FinsList sk.length) -> Type
-- public export
-- data CanAssign   : (sk: PortsList) -> (ports: FinsList sk.length) -> AssignsList sk ports -> Assign ports -> Type

-- public export
-- data AssignsList : (sk: PortsList) -> (ports: FinsList sk.length) -> Type where
--   Empty  : AssignsList sk []
--   Cons   : (a: Assign ports) -> (pre: AssignsList sk ports) -> CanAssign sk ports pre a -> AssignsList sk ports

-- public export
-- data EqBool : Bool -> Bool -> Type where
--   Same : (x : Bool) -> EqBool x x

-- public export
-- data FinNotIn : (FinsList n) -> (Fin n) -> Type where
--   FNIEmpty : FinNotIn [] f
--   FNICons  : {x, f: Fin n} -> (0 _ : So $ x /= f) -> (npi: FinNotIn xs f) -> FinNotIn (x :: xs) f

-- public export
-- toFinsList : AssignsList sk ports -> FinsList ports.length
-- toFinsList Empty = []
-- toFinsList (Cons a pre _) = case a of
--   Existing fin => fin :: toFinsList pre
--   New _        => toFinsList pre

-- public export
-- index : (fs : FinsList s) -> Fin fs.length -> Fin s
-- index (f::_ ) FZ     = f
-- index (_::fs) (FS i) = index fs i

-- public export
-- data CanAssign : (sk: PortsList) -> (ports: FinsList sk.length) -> AssignsList sk ports -> Assign ports -> Type where
--   YExistingSingle : {ports : FinsList sk.length} ->
--                     {assigns : AssignsList sk ports} ->
--                     {finPorts : Fin ports.length} ->
--                     EqBool False (multidriven' $ typeOf sk $ index ports finPorts) ->
--                     FinNotIn (toFinsList assigns) finPorts ->
--                     CanAssign sk ports assigns (Existing finPorts)
--   YExistingMulti  : {ports : FinsList sk.length} ->
--                     {assigns : AssignsList sk ports} ->
--                     {finPorts : Fin ports.length} ->
--                     EqBool True  (multidriven' $ typeOf sk $ index ports finPorts) ->
--                     CanAssign sk ports assigns (Existing finPorts)
--   YNew            : CanAssign sk ports assigns (New _)

-- public export
-- toPL : {sk: _} -> {ports: _} -> AssignsList sk ports -> PortsList
-- toPL Empty                     = []
-- toPL (Cons (Existing f) pre _) = typeOf sk (index ports f) :: toPL pre
-- toPL (Cons (New t)      pre _) = t :: toPL pre

-- public export
-- portIdx : {sk: _} -> {ports: _} -> AssignsList sk ports -> List $ Maybe $ Fin sk.length
-- portIdx Empty                     = []
-- portIdx (Cons (Existing f) pre _) = (Just $ index ports f) :: portIdx pre
-- portIdx (Cons (New t)      pre _) = Nothing :: portIdx pre

-- export
-- genAssigns : Fuel -> (sk: PortsList) -> (ports: FinsList sk.length) -> Gen MaybeEmpty $ AssignsList sk ports
