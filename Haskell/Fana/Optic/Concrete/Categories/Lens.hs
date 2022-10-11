module Fana.Optic.Concrete.Categories.Lens
(
	Lens (Lens), Lens',
	-- * Constructors
	lens_from_get_set, lens_from_get_lift,
	-- * Conversion from | to compact representation
	lens_from_sety_function, lens_to_sety_function,
	-- * Else
	lens_output_from_set_to_lift,
	lens_output_from_lift_to_set,
	-- * Some Instances
	lens_1, lens_2,
)
where

import Control.Arrow ((&&&))
import Fana.Haskell.DescribingClass
import Fana.Haskell.Wrap (Wrap (..))
import Fana.Math.Algebra.Category.Functor.Pro (Profunctor, dimap)
import Fana.Optic.Concrete.Categories.Interfaces
import Fana.Optic.Concrete.Combine
import Fana.Prelude.FromBase

import qualified Data.Constraint as Constraint
import qualified Data.Bifunctor as BiFr
import qualified Data.Either as Base
import qualified Fana.Data.HeteroPair as HePair
import qualified Fana.Haskell.Wrap as Wrap
import qualified Fana.Math.Algebra.Category.Functor.Pro as Profunctor
import qualified Fana.Math.Algebra.Category.OnTypePairs as Cat2
import qualified Fana.Optic.Profunctor.Interface as Proptic
import qualified Fana.Optic.Concrete.Common as Common


{-|
	The most straightforward representation of a lens is {get * set}.
	It is possible to merge these 2 accessor functions into 1 function.
	This change in the representation may bring asymtotically better running time
	[for example in recursive containers].
	This shows that a lens can be thought of as a function.
	To the codomain type of which i may refer to with "the output of the lens".
	Another representation of lens is {get * lift},
	where 'lift' is a higher order function,
	analogous to the 'lift' = 'fmap' function in library 'base'.
	I may refer to this representation as the "lifty" of "mappy" one.
	One advantage of the lifty representation is that
	its 'compose' function has a much cleaner implementation.
	This particular type uses an efficient [single function] representation.
-}
newtype Lens l1 l2 h1 h2 =
	{-
		This is the most straightforward efficient representation.
		I am still unsure if this is the best choice, hence i do not export the constructor.
	-}
	Lens (h1 -> (l1, l2 -> h2))
	deriving Functor
type Lens' l h = Common.Simple Lens l h

instance Wrap.Wrap (Lens l1 l2 h1 h2) where
	type Unwrap (Lens l1 l2 h1 h2) = h1 -> (l1, l2 -> h2)

-- converting between the 2 representations of the output of a lens :
lens_output_from_set_to_lift :: (x, y -> z) -> (x, (x -> y) -> z)
lens_output_from_set_to_lift i =
	let h1 = fst i
	in (h1, \ f -> snd i (f h1))
lens_output_from_lift_to_set :: (x, (x -> y) -> z) -> (x, y -> z)
lens_output_from_lift_to_set i =
	let h1 = fst i
	in (h1, \ y -> snd i (const y))

lens_from_get_set :: (h1 -> l1) -> (l2 -> h1 -> h2) -> Lens l1 l2 h1 h2
lens_from_get_set g s = Lens (g &&& flip s)

lens_from_get_lift :: (h1 -> l1) -> ((l1 -> l2) -> (h1 -> h2)) -> Lens l1 l2 h1 h2
lens_from_get_lift v l = Lens ((v &&& (flip l)) >>> lens_output_from_lift_to_set)

lens_from_sety_function :: (h1 -> (l1, l2 -> h2)) -> Lens l1 l2 h1 h2
lens_from_sety_function = Lens
lens_to_sety_function :: Lens l1 l2 h1 h2 -> h1 -> (l1, l2 -> h2)
lens_to_sety_function = unwrap


instance Profunctor (Lens l1 l2) where
	dimap f1 f2 (Lens l) = map f2 (Lens (f1 >>> l))

-- the following little things are helpers;
-- "e" = "early" and "l" = "late" versions;

move_load_inward_e :: (l, (l1, l2 -> h2)) -> (l1, l2 -> (l, h2))
move_load_inward_l :: ((l1, l2 -> h2), l) -> (l1, l2 -> (h2, l))
move_load_inward_e (l, (l1, f)) = (l1, f >>> HePair.after l)
move_load_inward_l ((l1, f), l) = (l1, f >>> HePair.before l)


instance Cat2.Category Lens where
	identity = Lens (id &&& const id)
	compose (Lens l12) (Lens l23) =
		let
			naked l3t1 =
				let
					r23 = l23 l3t1
					r12 = l12 (fst r23)
					in (fst r12, snd r12 >>> snd r23)
			in Lens naked

instance HasDescribingClass4 Lens where
	type DescribingClass4Of Lens = IsLens
	convert_from_describing_class_4 = down_and_replace >>> Lens

instance Profunctor.LoadableP (Lens l1 l2) where
	load_pe (Lens l) = Lens (Profunctor.load_pe l >>> move_load_inward_e)
	load_pl (Lens l) = Lens (Profunctor.load_pl l >>> move_load_inward_l)

instance Proptic.Adapted Lens where
	type ProfunctorConstraint Lens = Profunctor.LoadableP
	proof_of_constraint_implementation = Constraint.Dict
	from_adapted (Lens f) =
		-- :: tf l1 l2
		Profunctor.load_pl >>>
		-- :: tf (l1, l2 -> h2) (l2, l2 -> h2)
		Profunctor.dimap f (uncurry (flip ($)))
		-- :: tf h1 h2


instance IsFold Lens where
	to_list = down >>> (>>> pure)
	map_fold l f = down l >>> f
	fold_l l c r = down l >>> c r
	fold_r l c r = down l >>> flip c r
instance IsDown Lens where down = unwrap >>> (>>> fst)
instance IsFnUp Lens where fn_up (Lens l) ee h1 = let r = l h1 in snd r (ee (fst r))
instance IsTraversal Lens where
	traverse (Lens l) eae h1 = let r = l h1 in map (snd r) (eae (fst r))
instance IsAffineTraversal Lens where
	match_and_replace (Lens l) = l >>> BiFr.first Right
	match (Lens l) = l >>> fst >>> Right
	replace (Lens l) = l >>> snd
instance IsLens Lens where down_and_replace = unwrap


instance Sumable Lens where
	sum ::
		forall l1 l2 h11 h12 h21 h22 .
		(
			Lens l1 l2 h11 h12,
			Lens l1 l2 h21 h22
		) ->
		Lens l1 l2 (Either h11 h21) (Either h12 h22)
	sum (Lens l1, Lens l2) =
		let
			lens :: Either h11 h21 -> (l1, l2 -> Either h12 h22)
			lens = Base.either
				(l1 >>> (map >>> map) Left)
				(l2 >>> (map >>> map) Right)
			in Lens lens


-------------------------------------- some instances : ----------------------------------------------

lens_1 :: Lens l1 l2 (l1, y) (l2, y)
lens_1 = lens_from_get_set fst (\ e c -> (e, snd c))

lens_2 :: Lens l1 l2 (x, l1) (x, l2)
lens_2 = lens_from_get_set snd (\ e c -> (fst c, e))
