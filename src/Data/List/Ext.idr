module Data.List.Ext

import Data.List
import Data.Fin

public export
index : (l : List n) -> Fin (length l) -> n
index = index'

public export
(.asList) : List n -> List n
(.asList) = id
