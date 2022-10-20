module Fana.Optic.Concrete.Categories.Traversal
(
	Traversal (..),
	Traversal',
	from_Traversable,
)
where

import Data.Functor.Const (Const (..))
import Data.Functor.Identity (Identity (..))
import Fana.Haskell.DescribingClass
import Fana.Math.Algebra.Category.Functor.Pro
import Fana.Optic.Concrete.Categories.Interfaces
import Fana.Optic.Concrete.Combine
import Fana.Prelude hiding (traverse)

import qualified Data.Either as Base
import qualified Data.Traversable as Base
import qualified Fana.Math.Algebra.Category.OnTypePairs as Cat2
import qualified Fana.Optic.Concrete.Common as Common
import qualified Fana.Optic.Concrete.Categories.Fold as Fold


newtype Traversal l1 l2 h1 h2 = Traversal
	{
	unwrapTraversal :: forall a . Applicative a => (l1 -> a l2) -> (h1 -> a h2)
	}
type Traversal' l h = Common.Simple Traversal l h

instance Cat2.Category Traversal where
	identity = Traversal id
	compose (Traversal tr1) (Traversal tr2) = Traversal (tr1 >>> tr2)

instance HasDescribingClass4 Traversal where
	type DescribingClass4Of Traversal = IsTraversal
	convert_from_describing_class_4 tr = Traversal (traverse tr)


from_Traversable :: Traversable t => Traversal l1 l2 (t l1) (t l2)
from_Traversable = Traversal Base.traverse


instance Profunctor (Traversal t1 t2) where
	dimap f1 f2 (Traversal t) = Traversal (t >>> (dimap f1 (map f2)))
	map_i f1 (Traversal t) = Traversal (t >>> (f1 >>>))
	map_o f2 (Traversal t) = Traversal (t >>> (>>> map f2))

instance IsTraversal Traversal where traverse = unwrapTraversal

instance IsFnUp Traversal where
	fn_up (Traversal tr) = (>>> Identity) >>> tr >>> (>>> runIdentity)

instance IsFold Traversal where
	to_list tr = fold_r tr (:) []
	map_fold (Traversal tr) f = tr (f >>> Const) >>> getConst
	fold_r tr = Fold.fold_r_from_map_fold (map_fold tr)
	fold_l tr = Fold.fold_l_from_map_fold (map_fold tr)


-- * Combining Traversals by combinging their higher level

instance Productable Traversal where
	product ::
		forall l1 l2 h11 h12 h21 h22 .
		(Traversal l1 l2 h11 h12, Traversal l1 l2 h21 h22) ->
		Traversal l1 l2 (h11, h21) (h12, h22)
	product (Traversal trav_1, Traversal trav_2) = let
		trav ::
			forall a . Applicative a =>
			(l1 -> a l2) -> ((h11, h21) -> a (h12, h22))
		trav fl (h1, h2) = liftA2 (,) (trav_1 fl h1) (trav_2 fl h2)
		in Traversal trav

instance Sumable Traversal where
	sum ::
		forall l1 l2 h11 h12 h21 h22 .
		(Traversal l1 l2 h11 h12, Traversal l1 l2 h21 h22) ->
		Traversal l1 l2 (Either h11 h21) (Either h12 h22)
	sum (Traversal trav_1, Traversal trav_2) = let
		trav ::
			forall a . Applicative a =>
			(l1 -> a l2) -> Either h11 h21 -> a (Either h12 h22)
		trav fl = Base.either
			(trav_1 fl >>> map Left)
			(trav_2 fl >>> map Right)
		in Traversal trav
