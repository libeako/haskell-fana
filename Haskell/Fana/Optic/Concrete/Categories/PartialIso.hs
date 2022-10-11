-- | Partial isomorphism.
module Fana.Optic.Concrete.Categories.PartialIso
(
	PartialIso (..), PartialIso',
	lift_piso, add_for_failure,
	piso_convert_all, piso_convert_error, 
	piso_convert_error_with_low, piso_add_verification,
	test_piso,
)
where

import Control.Monad ((<=<))
import Fana.Haskell.DescribingClass
import Fana.Math.Algebra.Category.Functor.Pro
import Fana.Optic.Concrete.Categories.Interfaces
import Fana.Optic.Concrete.Categories.Iso (Iso')
import Fana.Prelude

import qualified Data.Bifunctor as Bifunctor
import qualified Data.Either as Either
import qualified Data.Foldable as Foldable
import qualified Data.Traversable as Traversable
import qualified Fana.Data.HeteroPair as Pair
import qualified Fana.Math.Algebra.Category.OnTypePairs as Category2
import qualified Fana.Optic.Concrete.Common as Common


{-|
	Partial isomorphism.
	Can represent for example serialization layers,
	whose parsers totally consume their input stream.
-}
data PartialIso e l1 l2 h1 h2 = PartialIso
	{
	piso_down :: h1 -> l1,
	piso_interpret :: l2 -> Either e h2
	}

type PartialIso' e l h = Common.Simple (PartialIso e) l h


instance Profunctor (PartialIso e t1 t2) where
	dimap f1 f2 (PartialIso d i) = PartialIso (f1 >>> d) (i >>> map f2)
	map_i f (PartialIso d i) = PartialIso (f >>> d) i
	map_o f (PartialIso d i) = PartialIso d (i >>> map f)

instance IsFold (PartialIso e) where
	to_list = piso_down >>> map (: [])
	fold_r o combine r h = combine (piso_down o h) r

instance IsDown (PartialIso e) where down = piso_down
instance IsInterpret e (PartialIso e) where interpret = piso_interpret
instance IsPartialIso e (PartialIso e) where

instance Category2.Category (PartialIso e) where
	identity = PartialIso id (id >>> Right)
	compose x1 x2 = PartialIso (down x2 >>> down x1) (interpret x2 <=< interpret x1)

instance HasDescribingClass4 (PartialIso e) where
	type DescribingClass4Of (PartialIso e) = IsPartialIso e
	convert_from_describing_class_4 o = PartialIso (down o) (interpret o)


piso_convert_error :: (e1 -> e2) -> PartialIso e1 l1 l2 h1 h2 -> PartialIso e2 l1 l2 h1 h2
piso_convert_error t x = x { piso_interpret = piso_interpret x >>> Bifunctor.first t }

piso_convert_error_with_low ::
	forall e1 e2 l1 l2 h1 h2 . (l2 -> e1 -> e2) -> PartialIso e1 l1 l2 h1 h2 -> PartialIso e2 l1 l2 h1 h2
piso_convert_error_with_low t m =
	let
		old_interpret = piso_interpret m
		new_interpret :: l2 -> Either e2 h2
		new_interpret i = Bifunctor.first (t i) (old_interpret i)
		in m { piso_interpret = new_interpret }

piso_convert_all ::
	(e1 -> e2) ->
	(l11 -> l12) ->
	(h21 -> h22) ->
	(h12 -> h11) ->
	(l22 -> l21) ->
	PartialIso e1 l11 l21 h11 h21 ->
	PartialIso e2 l12 l22 h12 h22
piso_convert_all e lr hp hr lp (PartialIso r p) =
	PartialIso (hr >>> r >>> lr) (lp >>> p >>> Bifunctor.bimap e hp)

piso_add_verification ::
	forall e l1 l2 h1 h2 . (h2 -> Maybe e) -> PartialIso e l1 l2 h1 h2 -> PartialIso e l1 l2 h1 h2
piso_add_verification get_error m =
	let
		old_interpret = piso_interpret m
		addition :: Either e h2 -> Either e h2
		addition =
			\case
				Left e -> Left e
				Right h ->
					case get_error h of
						Nothing -> Right h
						Just e -> Left e
		in m { piso_interpret = old_interpret >>> addition }


-- | Lifts the given PartialIso to work on a container of elements.
lift_piso :: (Functor tr, Traversable tp) => PartialIso e l1 l2 h1 h2 -> PartialIso e (tr l1) (tp l2) (tr h1) (tp h2)
lift_piso (PartialIso d i) = PartialIso (map d) (map i >>> Traversable.sequenceA)

{-|
	Adds data to the interpretation component of a partial isomorphism for the failure case.
	This may be useful for example in parsing,
	as added position information to the input is returned in error messages.
-}
add_for_failure :: PartialIso e l1 l2 h1 h2 -> PartialIso (d, e) l1 (d, l2) h1 h2
add_for_failure (PartialIso lr lp) =
	let
		move_into_error :: (d, Either e x) -> Either (d, e) x
		move_into_error (p, ei) = Bifunctor.first (Pair.after p) ei
		in PartialIso lr (Bifunctor.second lp >>> move_into_error)


----------------------- testing --------------------------


interpretation_fails :: PartialIso e l1 l2 h1 h2 -> l2 -> Bool
interpretation_fails layer = piso_interpret layer >>> Either.isLeft


{-|
	Goes down then up along the given partial isomorphism,
	on the given single data,
	checking to end where started,
	using the given isomorphisms at both levels.
-}
test_piso_on_single_data ::
	forall e l1 l2 h1 h2 . Eq h1 =>
	(Iso' l1 l2, Iso' h1 h2) -> h1 -> PartialIso e l1 l2 h1 h2 -> Bool
test_piso_on_single_data isos d piso =
	let
		round :: h1 -> Either e h1
		round =
			id
			>>> down piso
			>>> up (fst isos)
			>>> piso_interpret piso
			>>> map (down (snd isos))
		in Either.either (const False) (== d) (round d)

{-|
	Goes down then up along the given partial isomorphism
	and examines whether it ends back where started.

	This function tests both success and fail cases.
	Fail is expected on the given low level data.
	Success is expected on the given high level data.
-}
test_piso :: Eq h1 => (Iso' l1 l2, Iso' h1 h2) -> [l2] -> [h1] -> PartialIso e l1 l2 h1 h2 -> Bool
test_piso levels ls hs piso =
	Foldable.all (interpretation_fails piso) ls
	&&
	Foldable.all (flip (test_piso_on_single_data levels) piso) hs
