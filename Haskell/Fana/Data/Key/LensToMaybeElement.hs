module Fana.Data.Key.LensToMaybeElement
(
	LensAtKey, GetLensAtKey,
	HasLensToMaybeElement (..),
)
where

import Fana.Data.Key.ElementsHaveKey
import Fana.Prelude.FromBase
import Prelude (Ord)

import qualified Data.Maybe as Base hiding (mapMaybe)
import qualified Data.Map.Lazy as Base
import qualified Fana.Data.Filter as Filter
import qualified Fana.Data.Function as Fn
import qualified Fana.Optic.Concrete.Prelude as Optic
import qualified Fana.Optic.Laarhoven.Lens as LLens


type LensAtKey (m :: Type -> Type) v = Optic.Lens' (Maybe v) (m v)

-- | The type of function that gets focus on an element in an associative collection.
type GetLensAtKey t k e = k -> LensAtKey (t k) e


class
	(Ord (Key m), Traversable m, Filter.Filterable m) =>
	HasLensToMaybeElement m where

	lens_at :: Ord (Key m) => Key m -> LensAtKey m v

	get_at :: Ord (Key m) => Key m -> m e -> Maybe e
	get_at key m = Optic.down (lens_at key) m

	set_at :: Ord (Key m) => Key m -> e -> Fn.Endo (m e)
	set_at key value = Optic.fill (lens_at key) (Just value)

	delete_at :: Ord (Key m) => Key m -> Fn.Endo (m e)
	delete_at key = Optic.fill (lens_at key) Nothing

	-- | Sets to the given value if no value is there.
	ensure_existence_at :: Ord (Key m) => Key m -> e -> Fn.Endo (m e)
	ensure_existence_at key value m =
		let
			lens = lens_at key
			in if Base.isJust (Optic.down lens m) then m else Optic.fill lens (Just value) m

	contains_elem :: forall e . Eq e => Key m -> e -> m e -> Bool
	contains_elem key value m = maybe False (== value) (get_at key m)

	{-# MINIMAL lens_at #-}


-- instances

instance Ord key => HasLensToMaybeElement (Base.Map key) where
	lens_at key = LLens.to_concrete (flip Base.alterF key)
