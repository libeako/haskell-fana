module Fana.Prelude.FromBase
(
	type (~), (.), const, flip, curry, uncurry, ($),
	Type, Constraint,
	Eq, (==), (/=),
	Bool (..), (&&), (||),
	fst, snd,
	Semigroup (..), Monoid (..),
	Functor, Foldable (toList, fold, foldMap, foldr, foldl'), Traversable, traverse, sequenceA,
	Bifunctor, bimap,
	Applicative (..), liftA3,
	Monad (..), (>=>), (=<<),
	Maybe (..), maybe, fromMaybe, catMaybes,
	Either (..), either, partitionEithers,
	map,
	id, (>>>), (<<<),
	Coercible, coerce,
)
where

import Control.Applicative (Applicative (..), pure, liftA2, liftA3)
import Control.Category
import Control.Monad (Monad (..), (>=>), (>>=), (=<<))
import Data.Bifunctor (Bifunctor, bimap)
import Data.Bool as Bool
import Data.Coerce (Coercible, coerce)
import Data.Either (Either (..), either, partitionEithers)
import Data.Functor (Functor (..))
import Data.Foldable
import Data.Kind (Type, Constraint)
import Data.Maybe (Maybe (..), maybe, fromMaybe, catMaybes)
import Data.Semigroup (Semigroup (..))
import Data.Monoid (Monoid (..))
import Data.Traversable (Traversable, traverse, sequenceA)
import Data.Type.Equality(type (~))
import Prelude (const, flip, curry, uncurry)
import Prelude (($), Eq, (==), (/=), fst, snd)


map :: Functor f => (x -> y) -> (f x -> f y)
map = fmap
