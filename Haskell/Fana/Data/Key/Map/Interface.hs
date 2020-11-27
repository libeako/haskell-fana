module Fana.Data.Key.Map.Interface
(
	MapContainer (..),
)
where

import Fana.Data.CollectionWithEmpty
import Fana.Data.Filter
import Fana.Data.Key.ElementsHaveKey
import Fana.Data.Key.LensToMaybeElement
import Fana.Data.Key.Traversable
import Fana.Prelude.FromBase
import Prelude (Ord)

import qualified Data.Foldable as Fold
import qualified Data.Map.Lazy as Base
import qualified Data.Maybe as Maybe
import qualified Data.Ord as Base
import qualified Fana.Optic.Concrete.Prelude as Optic


class
		(Filterable t, TraversableWithKeyAndDelete t, HasLensToMaybeElement t, CollWithEmpty t) =>
		MapContainer t
		where

	from_list :: Ord (Key t) => [(Key t, e)] -> t e
	from_list = Fold.foldr (uncurry set_at) empty_coll

	-- | Same as @from_list@ but creates list of values for each key.
	from_list_lists :: forall e . Ord (Key t) => [(Key t, e)] -> t [e]
	from_list_lists =
		let
			folder :: (Key t, e) -> t [e] -> t [e]
			folder (name, value) =
				let
					updater :: Maybe [e] -> Maybe [e]
					updater = Maybe.fromMaybe [] >>> (value : ) >>> Just
					in Optic.fn_up (lens_at name) updater
			in Fold.foldr folder empty_coll

	{- |
		Same as 'from_list' but assumes and verifies that the keys in the given input list are unique.

		In case of key collision : the output is a key at which collision is.
	-}
	from_list_of_uniques :: forall e . Ord (Key t) => [(Key t, e)] -> Either (Key t) (t e)
	from_list_of_uniques =
		let
			elem_singlifier :: Key t -> [e] -> Either (Key t) e
			elem_singlifier key =
				\ case
					[e] -> Right e
					_ -> Left key
			in from_list_lists >>> traverse_with_key elem_singlifier



-- instances

instance Base.Ord k => MapContainer (Base.Map k) where
