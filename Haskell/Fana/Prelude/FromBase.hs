module Fana.Prelude.FromBase
(
	(.), const, flip, curry, uncurry, ($),
	Type, Constraint,
	Eq, (==), (/=),
	Bool (..), (&&), (||),
	fst, snd,
	Semigroup (..), Monoid (..),
	Functor, Foldable (toList, fold, foldMap, foldr, foldl'), Traversable, traverse, sequenceA,
	Bifunctor, bimap,
	Applicative (..), liftA3,
	Monad (..),
	Maybe (..), maybe,
	Either (..), either,
	map,
	id, (>>>), (<<<),
	Coercible, coerce,
)
where

import Control.Applicative (Applicative (..), liftA2, liftA3)
import Control.Category
import Control.Monad (Monad (..))
import Data.Bifunctor (Bifunctor, bimap)
import Data.Bool as Bool
import Data.Coerce (Coercible, coerce)
import Data.Either (Either (..), either)
import Data.Functor (Functor (..))
import Data.Foldable
import Data.Kind (Type, Constraint)
import Data.Maybe (Maybe (..), maybe)
import Data.Semigroup (Semigroup (..))
import Data.Monoid (Monoid (..))
import Data.Traversable (Traversable, traverse, sequenceA)
import Prelude (const, flip, curry, uncurry)
import Prelude (($), Eq, (==), (/=), fst, snd)


map :: Functor f => (x -> y) -> (f x -> f y)
map = fmap
