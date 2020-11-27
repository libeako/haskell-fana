module Fana.Data.CollectionWithEmpty
(
	CollWithEmpty (..),
)
where

import Fana.Prelude.FromBase

import qualified Data.List as BaseList
import qualified Data.Map.Lazy as BaseMap
import qualified Data.Maybe as Base


{-| Collection type that has an @empty@ constructor and emptiness can be tested. -}
class CollWithEmpty t where
	empty_coll :: t e
	is_coll_empty :: t e -> Bool


-- instances

instance CollWithEmpty Maybe where
	empty_coll = Nothing
	is_coll_empty = Base.isNothing

instance CollWithEmpty [] where
	empty_coll = []
	is_coll_empty = BaseList.null

instance CollWithEmpty (BaseMap.Map k) where
	empty_coll = BaseMap.empty
	is_coll_empty = BaseMap.null
