module Fana.Data.Key.ElementsHaveKey
(
	ElementsHaveKey (..),
)
where

import Fana.Prelude.FromBase
import Data.Map.Lazy as Base


{-| Collections, whose elements have keys. -}
class ElementsHaveKey t where
	type Key t :: Type


-- instances

instance ElementsHaveKey (Base.Map k) where
	type Key (Base.Map k) = k

