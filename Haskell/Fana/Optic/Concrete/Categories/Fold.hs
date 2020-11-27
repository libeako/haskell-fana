module Fana.Optic.Concrete.Categories.Fold
(
	Fold, Fold',
	from_to_list, from_map_fold, from_Foldable,
	fold_l_from_map_fold, fold_r_from_map_fold
)
where

import Control.Monad ((<=<))
import Data.Foldable (Foldable)
import Fana.Haskell.DescribingClass
import Fana.Math.Algebra.Category.Functor.Pro
import Fana.Optic.Concrete.Categories.Interfaces
import Fana.Prelude.FromBase
import Prelude (Monoid, pure)

import qualified Data.Foldable as Foldable
import qualified Data.Semigroup as Sg
import qualified Fana.Haskell.Wrap as Wrap
import qualified Fana.Math.Algebra.Category.OnTypePairs as Cat2
import qualified Fana.Optic.Concrete.Common as Common


{-
	The choice of representation is not sure.
	It can be tolist, mapfold, foldr.
	Hence the constructor is not exported.
-}
newtype Fold l1 l2 h1 h2 = Fold { deFold :: h1 -> [l1] }
type Fold' l h = Common.Simple Fold l h

instance Wrap.Wrap (Fold l1 l2 h1 h2) where
	type Unwrap (Fold l1 l2 h1 h2) = h1 -> [l1]

instance Cat2.Category Fold where
	empty = Fold pure
	compose = Wrap.over_2 (<=<)

instance HasDescribingClass4 Fold where
	type DescribingClass4Of Fold = IsFold
	convert_from_describing_class_4 = to_list >>> from_to_list


from_Foldable :: Foldable h1 => Fold l1 l2 (h1 l1) h2
from_Foldable = Fold Foldable.toList

from_map_fold :: (forall m . Monoid m => (l1 -> m) -> (h1 -> m)) -> Fold l1 l2 h1 h2
from_map_fold f = Fold (f pure)

from_to_list :: (h1 -> [l1]) -> Fold l1 l2 h1 h2
from_to_list = Fold


instance Profunctor (Fold t1 t2) where
	dimap f1 _ (Fold f) = Fold (f1 >>> f)
	map_i f1 = Wrap.over (f1 >>>)
	map_o _ (Fold f) = Fold f

instance IsFold Fold where
	to_list = deFold


fold_r_from_map_fold ::
	(forall m . Monoid m => (l -> m) -> (h -> m)) ->
	(l -> r -> r) -> r -> h -> r
fold_r_from_map_fold mf combine r h = Sg.appEndo (mf (combine >>> Sg.Endo) h) r

fold_l_from_map_fold ::
	(forall m . Monoid m => (l -> m) -> (h -> m)) ->
	(r -> l -> r) -> r -> h -> r
fold_l_from_map_fold mf combine r h =
	(Sg.getDual >>> Sg.appEndo) (mf (flip combine >>> (Sg.Endo >>> Sg.Dual)) h) r
