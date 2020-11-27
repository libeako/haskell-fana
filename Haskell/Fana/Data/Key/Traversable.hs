module Fana.Data.Key.Traversable
(
	TraversableWithKey (..),
	TraversableWithKeyAndDelete (..),
)
where


import Data.Functor.Identity
import Fana.Data.Filter
import Fana.Data.Key.ElementsHaveKey
import Fana.Prelude.FromBase

import qualified Data.Map as OfBase
import qualified Fana.Data.HeteroPair as Pair


{-|
	Same as base/Traversable,
	but the element converter function has access to the key of the element.
-}
class (Functor t, Foldable t, ElementsHaveKey t) => TraversableWithKey t where
	traverse_with_key :: Applicative a => (Key t -> x -> a y) -> t x -> a (t y)
	map_with_key :: (Key t -> x -> y) -> t x -> t y
	map_with_key =
		(>>> (>>> Identity))
		>>> traverse_with_key >>>
		(>>> runIdentity)

	key_value_pairs :: t e -> [(Key t, e)]
	key_value_pairs = map_with_key Pair.make >>> toList

	keys :: t e -> [Key t]
	keys = key_value_pairs >>> map fst


{-|
	Same as TraversableWithKey,
	but also deletes the elements
	to which the transformator function maps @Nothing@.
-}
class (TraversableWithKey t, Filterable t) => TraversableWithKeyAndDelete t where
	traverse_with_key_and_delete :: Applicative a => (Key t -> x -> a (Maybe y)) -> t x -> a (t y)
	traverse_with_key_and_delete = traverse_with_key >>> (>>> map filter)
	map_with_key_and_delete :: (Key t -> x -> Maybe y) -> t x -> t y
	map_with_key_and_delete =
		(>>> (>>> Identity))
		>>> traverse_with_key_and_delete >>>
		(>>> runIdentity)

	{-|
		Verifies each element contains at most 1 element.
		Otherwise returns a non-single element as error.
		Empty elements are okay, they are deleted.
	-}
	singlify_element_lists :: forall e . t [e] -> Either (Key t, [e]) (t e)
	singlify_element_lists =
		let
			per_element :: Key t -> [e] -> Either (Key t, [e]) (Maybe e)
			per_element key value_list =
				case value_list of
					[] -> Right Nothing
					[single] -> Right (Just single)
					_ : _ : _ -> Left (key, value_list)
			in traverse_with_key_and_delete per_element


-- instances

instance TraversableWithKey (OfBase.Map k) where
	traverse_with_key = OfBase.traverseWithKey

instance TraversableWithKeyAndDelete (OfBase.Map k) where
	traverse_with_key_and_delete = OfBase.traverseMaybeWithKey
	map_with_key_and_delete = OfBase.mapMaybeWithKey
