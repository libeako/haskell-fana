module Fana.Optic.Concrete.Categories.Iso
(
	Iso (Iso), Iso',
	reverse,
	fn_down,
	change_iso_per_component,
	iso_up,
	-- * Some Instances
	iso_with_nothing_before,
	iso_pair_swap, iso_Either_swap,
	iso_of_wrapping,
	-- * Else
	test_iso_law,
)
where

import Fana.Haskell.DescribingClass
import Fana.Math.Algebra.Category.Functor.Pro (Profunctor, dimap)
import Fana.Optic.Concrete.Categories.Interfaces
import Fana.Prelude

import qualified Data.Constraint as Constraint
import qualified Data.Either as Base
import qualified Data.Foldable as Base
import qualified Fana.Haskell.Wrap as Wrap
import qualified Fana.Math.Algebra.Category.OnTypePairs as Cat2
import qualified Fana.Data.HeteroPair as Pair
import qualified Fana.Optic.Concrete.Common as Common
import qualified Fana.Optic.Profunctor.Interface as Proptic



-- | Isomorphism
data Iso l1 l2 h1 h2 = Iso
	{
	in_iso_down :: h1 -> l1,
	in_iso_up :: l2 -> h2
	}
type Iso' l h = Common.Simple Iso l h

reverse :: Iso l1 l2 h1 h2 -> Iso h2 h1 l2 l1
reverse (Iso d u) = Iso u d

fn_down :: Iso l1 l2 h1 h2 -> (h2 -> h1) -> (l2 -> l1)
fn_down iso hf = in_iso_up iso >>> hf >>> in_iso_down iso


-- | Changes an isomorphism by changing its components independently
change_iso_per_component ::
	((xc1 -> xe1) -> (yc1 -> ye1)) ->
	((xe2 -> xc2) -> (ye2 -> yc2)) ->
	Iso xe1 xe2 xc1 xc2 ->
	Iso ye1 ye2 yc1 yc2
change_iso_per_component change_down change_up (Iso d u) = Iso (change_down d) (change_up u)

iso_up :: Functor f => Iso l1 l2 h1 h2 -> Iso (f l1) (f l2) (f h1) (f h2)
iso_up = change_iso_per_component map map


instance Cat2.Category Iso where
	empty = Iso id id
	compose (Iso d12 u12) (Iso d23 u23) = Iso (d23 >>> d12) (u12 >>> u23)

instance HasDescribingClass4 Iso where
	type DescribingClass4Of Iso = IsIso
	convert_from_describing_class_4 o = Iso (down o) (up o)


instance IsFold Iso where
	to_list = in_iso_down >>> (>>> pure)
	map_fold o f = in_iso_down o >>> f
	fold_l o c r = in_iso_down o >>> c r
	fold_r o c r = in_iso_down o >>> flip c r
instance IsDown Iso where down = in_iso_down
instance IsInterpret e Iso where interpret = in_iso_up >>> map Right
instance IsUp Iso where up = in_iso_up
instance IsFnUp Iso where fn_up o ee = in_iso_down o >>> ee >>> in_iso_up o
instance IsTraversal Iso where traverse o eae = in_iso_down o >>> eae >>> map (in_iso_up o)
instance IsAffineTraversal Iso where
	match_and_replace (Iso d u) = liftA2 (,) (d >>> Right) (const u)
	match iso = in_iso_down iso >>> Right
	replace iso = const (in_iso_up iso)
instance IsLens Iso where
	down_and_replace (Iso d u) = \ c -> (d c, u)
instance IsPrism Iso where
instance IsPartialIso e Iso where
instance IsIso Iso where


instance Profunctor (Iso l1 l2) where
	dimap fi fo (Iso d u) = Iso (fi >>> d) (u >>> fo)


-- | Test of the given function being the identity function.
test_identity_of_function :: Eq t => (t -> t) -> [t] -> Bool
test_identity_of_function f = Base.all (\ x -> x == f x)

test_iso_law :: (Eq l, Eq h) => Iso l l h h -> ([h], [l]) -> Bool
test_iso_law (Iso d u) (highs, lows) =
	test_identity_of_function (d >>> u) highs
	&&
	test_identity_of_function (d <<< u) lows

iso_of_wrapping ::
	(Wrap.Wrap h1, Wrap.Wrap h2) => Iso (Wrap.Unwrap h1) (Wrap.Unwrap h2) h1 h2
iso_of_wrapping = Iso Wrap.unwrap Wrap.wrap

instance Proptic.Adapted Iso where
	type ProfunctorConstraint Iso = Profunctor
	proof_of_constraint_implementation = Constraint.Dict
	from_adapted c = dimap (in_iso_down c) (in_iso_up c)



------------------------------------ some instances ---------------------------

iso_with_nothing_before :: Iso l1 l2 ((), l1) ((), l2)
iso_with_nothing_before = Iso snd (Pair.after ())

iso_pair_swap :: Iso (x1, y1) (x2, y2) (y1, x1) (y2, x2)
iso_pair_swap = Iso Pair.swap Pair.swap
iso_Either_swap :: Iso (Either x1 y1) (Either x2 y2) (Either y1 x1) (Either y2 x2)
iso_Either_swap = Iso (Base.either Right Left) (Base.either Right Left)
