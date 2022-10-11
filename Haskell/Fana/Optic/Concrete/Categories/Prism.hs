module Fana.Optic.Concrete.Categories.Prism
(
	Prism (Prism), Prism',
	from_up_and_match,
	-- * Some Instances
	prism_Maybe,
	prism_Left,
	prism_Right,
)
where

import Fana.Haskell.DescribingClass
import Fana.Math.Algebra.Category.Functor.Pro(Profunctor, dimap)
import Fana.Optic.Concrete.Categories.Interfaces
import Fana.Prelude.FromBase

import qualified Data.Bifunctor as BiFr
import qualified Data.Constraint as Constraint
import qualified Data.Foldable as Foldable
import qualified Fana.Math.Algebra.Category.Functor.Pro as Profunctor
import qualified Fana.Math.Algebra.Category.OnTypePairs as Cat2
import qualified Fana.Optic.Concrete.Common as Common
import qualified Fana.Optic.Profunctor.Interface as Proptic



data Prism l1 l2 h1 h2 = Prism
	{
	p_match :: h1 -> Either h2 l1,
	p_build :: l2 -> h2
	}
type Prism' l h = Common.Simple Prism l h

from_up_and_match :: (l2 -> h2) -> (h1 -> Either h2 l1) -> Prism l1 l2 h1 h2
from_up_and_match = flip Prism

continue_match :: Prism l1 l2 h1 h2 -> r -> (l1 -> r) -> h1 -> r
continue_match p r f = p_match p >>> either (const r) f

instance Cat2.Category Prism where
	identity = Prism Right id
	compose :: forall p1 p2 q1 q2 r1 r2 . Prism p1 p2 q1 q2 -> Prism q1 q2 r1 r2 -> Prism p1 p2 r1 r2
	compose pq qr =
		let
			new_match :: r1 -> Either r2 p1
			new_match r1 =
				(p_match qr r1 :: Either r2 q1)
				 >>= ((p_match pq >>> BiFr.first (up qr)) :: q1 -> Either r2 p1)
			new_build :: p2 -> r2
			new_build = up pq >>> up qr
			in Prism new_match new_build

instance HasDescribingClass4 Prism where
	type DescribingClass4Of Prism = IsPrism
	convert_from_describing_class_4 o = Prism (match o) (up o)


instance Profunctor (Prism p1 p2) where
	dimap f1 f2 p = Prism (f1 >>> p_match p >>> BiFr.first f2) (up p >>> f2)

-- the following little things are helpers;
-- "e" = "early" and "l" = "late" versions;

move_in_l :: Either (Either a b) x -> Either (Either a x) b
move_in_e :: Either x (Either a b) -> Either (Either x a) b
move_in_l = either (either (Left <<< Left) Right) (Left <<< Right)
move_in_e = either (Left <<< Left) (either (Left <<< Right) Right)

instance Profunctor.LoadableS (Prism p1 p2) where
	-- with p = Prism p1 p2 :
	--   load_se :: p a b -> p (Either c a) (Either c b)
	--   load_sl :: p a b -> p (Either a c) (Either b c)
	load_se p = Prism (BiFr.second (p_match p) >>> move_in_e) (up p >>> Right)
	load_sl p = Prism (BiFr.first (p_match p) >>> move_in_l) (up p >>> Left)


instance Proptic.Adapted Prism where
	type ProfunctorConstraint Prism = Profunctor.LoadableS
	proof_of_constraint_implementation = Constraint.Dict
	from_adapted c =
		Profunctor.load_se >>>
		Profunctor.dimap (p_match c) (either id (up c))


instance IsFold Prism where
	to_list = p_match >>> (>>> Foldable.toList)
	map_fold p f = continue_match p mempty f
	fold_l p h r = continue_match p r (h r)
	fold_r p h r = continue_match p r (flip h r)
instance IsInterpret e Prism where interpret = p_build >>> map Right
instance IsUp Prism where up = p_build
instance IsFnUp Prism where
	fn_up (Prism m b) ee h1 = either id (ee >>> b) (m h1)
	fill = p_build >>> (>>> const)
instance IsTraversal Prism where
	traverse (Prism m b) eae h1 = either pure (eae >>> map b) (m h1)
instance IsAffineTraversal Prism where
	match_and_replace (Prism m b) l1 = (m l1, b)
	match = p_match
	replace = p_build >>> const
instance IsPrism Prism where


------------------------ some instances -------------------------------

prism_Maybe :: Prism l1 l2 (Maybe l1) (Maybe l2)
prism_Maybe = from_up_and_match Just (maybe (Left Nothing) Right)

prism_Left :: Prism l1 l2 (Either l1 r) (Either l2 r)
prism_Left = from_up_and_match Left (either Right (Right >>> Left))

prism_Right :: Prism l1 l2 (Either l l1) (Either l l2)
prism_Right = from_up_and_match Right (either (Left >>> Left) Right)

