{-| Map of which the type of keys is string of characters. -}
module Fana.Data.Key.Map.KeyIsString
(
	Elem, Map (..),
)
where

import Data.Default.Class
import Fana.Data.CollectionWithEmpty
import Fana.Data.Key.ElementsHaveKey
import Fana.Data.Key.Map.Interface
import Fana.Data.Key.LensToMaybeElement (HasLensToMaybeElement (..))
import Fana.Data.Key.Traversable
import Fana.Data.Zippable (Zippable (..))
import Fana.Math.Algebra.Category.ConvertThenCompose ((>**>^))
import Fana.Prelude hiding (traverse)
import Prelude (Ord)

import qualified Control.Monad as Base
import qualified Data.List as List
import qualified Data.Map.Lazy as Base
import qualified Data.Maybe as Maybe
import qualified Data.Monoid as Base
import qualified Data.Semigroup as Base
import qualified Data.Traversable as Base
import qualified Fana.Data.Filter as Filter
import qualified Fana.Math.Algebra.Category.OnTypePairs as Category2
import qualified Fana.Optic.Concrete.Prelude as Optic


{-| Element type of the map, just a key-value pair. -}
type Elem c e = ([c], e)

{-|
	Map data type of which the type of keys is string of characters.
	Maps strings of c-s to e-s. Also known as "trie".

	* 'c' is the type of character in the key string.
	* 'e' is the type of elements of the container.
-}
data Map c e = 
	Joint { immediate_value :: Maybe e, children :: Base.Map c (Map c e) }
	deriving (Eq, Functor, Foldable, Traversable)


-- | Lens to the immediate_value of a Map.
lens_imm_value :: Optic.Lens' (Maybe e) (Map c e)
lens_imm_value = Optic.lens_from_get_set immediate_value (\ iv m -> m { immediate_value = iv })

-- | Lens to the children of a Map.
lens_children :: Optic.Lens' (Base.Map c (Map c e)) (Map c e)
lens_children  = Optic.lens_from_get_set children (\ cs m -> m { children = cs })


instance CollWithEmpty (Map c) where
	empty_coll = Joint Nothing Base.empty
	is_coll_empty = liftA2 (&&) (immediate_value >>> Maybe.isNothing) (children >>> Base.null)

instance Default (Map c e) where def = empty_coll

maybefy :: Map c e -> Maybe (Map c e)
maybefy m = if is_coll_empty m then Nothing else Just m

instance Ord key => Zippable (Map key) where
	zip mfx mfy fxy x y =
		let
			new_iv = zip mfx mfy fxy (immediate_value x) (immediate_value y)
			new_cs = zip (map map mfx) (map map mfy) (zip mfx mfy fxy) (children x) (children y)
			in Joint new_iv new_cs

instance (Ord c, Semigroup e) => Semigroup (Map c e) where
	(<>) = zip (Just id) (Just id) (Base.<>)
instance (Ord c, Base.Semigroup e) => Monoid (Map c e) where mempty = empty_coll

instance Ord c => HasLensToMaybeElement (Map c) where
	lens_at =
		\ case
			[] -> lens_imm_value
			c : rest ->
				let
					{-|
						If a map is empty then
						it as element should be deleted from its container
						to save running time and memory.
					-}
					optimize :: Map c e -> Maybe (Map c e)
					optimize m = if is_coll_empty m then Nothing else Just m
					maybeize :: Optic.Lens' (Map c e) (Maybe (Map c e))
					maybeize = Optic.lens_from_get_set (Maybe.fromMaybe empty_coll) (optimize >>> const)
					in
						Category2.empty
						>**>^ lens_at rest
						>**>^ maybeize
						>**>^ lens_at c
						>**>^ lens_children


-- traverse

{-|
	.

	The element processing function takes a reverse list of characters of the key.
-}
traverse'' ::
	forall a c ei eo .
	Applicative a =>
	[c] -> ([c] -> ei -> a (Maybe eo)) -> Map c ei -> a (Maybe (Map c eo))
traverse'' path f (Joint ie children') =
	let
		elem_processor :: c -> Map c ei -> a (Maybe (Map c eo))
		elem_processor char = traverse'' (char : path) f
		possibly_empty :: a (Map c eo)
		possibly_empty =
			liftA2 Joint (map Base.join (Base.traverse (f path) ie))
				(Base.traverseMaybeWithKey elem_processor children')
		in map maybefy possibly_empty

traverse' :: Applicative a => ([c] -> ei -> a (Maybe eo)) -> Map c ei -> a (Map c eo)
traverse' f = traverse'' [] (List.reverse >>> f) >>> map (Maybe.fromMaybe empty_coll)

instance ElementsHaveKey (Map c) where
	type Key (Map c) = [c]

instance TraversableWithKey (Map c) where
	traverse_with_key = (>>> (>>> map Just)) >>> traverse'
instance TraversableWithKeyAndDelete (Map c) where
	traverse_with_key_and_delete = traverse'

instance Filter.Filterable (Map c) where
	separate :: forall l r . Map c (Either l r) -> (Map c l, Map c r)
	separate (Joint ie children') =
		let
			new_ie = Filter.separate ie
			new_children :: (Base.Map c (Map c l), Base.Map c (Map c r))
			new_children = (liftA2 (,) (map fst) (map snd)) (map Filter.separate children')
			output_l = Joint (fst new_ie) (fst new_children)
			output_r = Joint (snd new_ie) (snd new_children)
			in (output_l, output_r)

instance Ord c => MapContainer (Map c) where
