module Fana.Math.Algebra.Monoid.Accumulate
(
	Accumulated,
	single, extract,
)
where

import Fana.Prelude.FromBase

import qualified Data.String as Base
import qualified Fana.Data.Tree.Leaf as Leafy
import qualified Prelude as Base


{-|
	Adds fast composition, regardless of the associtivity of the composition, to its constituent type.
	The type parameter "c" is the "constituent" type that this type builds on.
-}
newtype Accumulated c = Accumulated { unwrapAccumulated :: [Leafy.Tree [] c] }
	deriving (Functor, Foldable, Traversable)

instance Semigroup (Accumulated b) where
	Accumulated x <> Accumulated y = Accumulated [(Leafy.joint x), (Leafy.joint y)]
instance Monoid (Accumulated b) where mempty = Accumulated []

single :: c -> Accumulated c
single = Leafy.leaf >>> (: []) >>> Accumulated

instance Applicative Accumulated where
	pure = single
	liftA2 f (Accumulated x) (Accumulated y) = Accumulated (liftA2 (liftA2 f) x y)
instance Monad Accumulated where
	return = pure
	Accumulated x >>= f = Accumulated (map (>>= (f >>> unwrapAccumulated >>> Leafy.joint)) x)

extract :: Monoid c => Accumulated c -> c
extract = unwrapAccumulated >>> foldMap fold

instance (Monoid b, Base.Show b) => Base.Show (Accumulated b) where show = extract >>> Base.show
instance Base.IsString b => Base.IsString (Accumulated b) where fromString = Base.fromString >>> single
