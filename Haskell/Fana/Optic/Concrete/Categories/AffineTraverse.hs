module Fana.Optic.Concrete.Categories.AffineTraverse
(
	AffineTraversal (..), AffineTraversal', to_AffineTraversal,
)
where

import Control.Arrow ((&&&))
import Fana.Haskell.DescribingClass
import Fana.Math.Algebra.Category.Functor.Pro (Profunctor)
import Fana.Optic.Concrete.Categories.Interfaces
import Fana.Optic.Concrete.Categories.Iso (Iso (Iso))
import Fana.Prelude

import qualified Data.Bifunctor as BiFr
import qualified Data.Constraint as Constraint
import qualified Fana.Data.HeteroPair as Pair
import qualified Fana.Math.Algebra.Category.Functor.Pro as Profunctor
import qualified Fana.Math.Algebra.Category.OnTypePairs as Cat2
import qualified Fana.Optic.Concrete.Common as Common
import qualified Fana.Optic.Profunctor.Interface as OpticP




data AffineTraversal l1 l2 h1 h2 = AffineTraversal
	{ unwrapAffineTraverse :: h1 -> (Either h2 l1, l2 -> h2) }
type AffineTraversal' l h = Common.Simple AffineTraversal l h

-- | Isomorphism to an AffineTraversal from its unwrapped content.
iso_AffineTraversal ::
	Iso
		(xh1 -> (Either xh2 xl1, xl2 -> xh2)) (yh1 -> (Either yh2 yl1, yl2 -> yh2))
		(AffineTraversal xl1 xl2 xh1 xh2) (AffineTraversal yl1 yl2 yh1 yh2)
iso_AffineTraversal = Iso unwrapAffineTraverse AffineTraversal

-- | Constructs the output of the composed traversal in case of mismatch.
output_strictly :: c -> (Either c y, x -> c)
output_strictly c = (Left c, const c)

instance Cat2.Category AffineTraversal where
	identity = AffineTraversal (Right &&& (const id))
	compose ::
		forall p1 p2 q1 q2 r1 r2 .
		AffineTraversal p1 p2 q1 q2 ->
		AffineTraversal q1 q2 r1 r2 ->
		AffineTraversal p1 p2 r1 r2
	compose (AffineTraversal pq) (AffineTraversal qr) = AffineTraversal new_trav
		where
			new_trav :: r1 -> (Either r2 p1, p2 -> r2)
			new_trav r1 =
				let output_qr = qr r1 in
					case fst output_qr of
						Left r2 -> output_strictly r2
						Right q1 ->
							let output_pq = pq q1 in
								case fst output_pq of
									Left q2 -> output_strictly (snd output_qr q2)
									Right p1 -> (Right p1, snd output_pq >>> snd output_qr)

instance HasDescribingClass4 AffineTraversal where
	type DescribingClass4Of AffineTraversal = IsAffineTraversal
	convert_from_describing_class_4 = match_and_replace >>> AffineTraversal


instance IsAffineTraversal AffineTraversal where
	match_and_replace = unwrapAffineTraverse

instance IsTraversal AffineTraversal where
	traverse (AffineTraversal trav) e h1 =
		let output = trav h1 in
			either pure (e >>> map (snd output)) (fst output)

instance IsFnUp AffineTraversal where
	fn_up (AffineTraversal trav) e h1 =
		let output = trav h1 in
			either id (e >>> snd output) (fst output)

instance IsFold AffineTraversal where
	to_list (AffineTraversal trav) = trav >>> fst >>> either (const []) pure
	map_fold (AffineTraversal trav) f = trav >>> fst >>> either mempty f
	fold_l (AffineTraversal trav) h r = trav >>> fst >>> either (const r) (h r)
	fold_r (AffineTraversal trav) h r = trav >>> fst >>> either (const r) (flip h r)


instance Profunctor (AffineTraversal l1 l2) where
	dimap fi fo = fn_up iso_AffineTraversal (Profunctor.dimap fi (BiFr.bimap (BiFr.first fo) (map fo)))

load_pe_output :: l -> (Either h2 l1, l2 -> h2) -> (Either (l, h2) l1, l2 -> (l, h2))
load_pe_output l = BiFr.bimap (BiFr.first (Pair.after l)) (map (Pair.after l))

load_se_output :: Either l (Either h2 l1, l2 -> h2) -> (Either (Either l h2) l1, l2 -> Either l h2)
load_se_output = either
	((Left >>> Left) &&& (Left >>> const))
	(BiFr.bimap (BiFr.first Right) (map Right))

instance Profunctor.LoadableP (AffineTraversal l1 l2) where
	load_pe = fn_up iso_AffineTraversal (Profunctor.load_pe >>> map (uncurry load_pe_output))

instance Profunctor.LoadableS (AffineTraversal l1 l2) where
	load_se = fn_up iso_AffineTraversal (Profunctor.load_se >>> map load_se_output)

instance Profunctor.LoadablePS (AffineTraversal l1 l2) where


extract' :: (h1 -> l2 -> h2) -> (h1, Either h2 l2) -> h2
extract' put_ ei = either id (flip put_ >>> ($ (fst ei))) (snd ei)

instance OpticP.Adapted AffineTraversal where
	type ProfunctorConstraint AffineTraversal = Profunctor.LoadablePS
	proof_of_constraint_implementation = Constraint.Dict
	from_adapted (AffineTraversal trav) =
		Profunctor.load_se >>>
		Profunctor.map_i (trav >>> fst) >>>
		Profunctor.load_pe >>>
		(Profunctor.dimap (\x -> (x, x)) (extract' (trav >>> snd)))

to_AffineTraversal :: IsAffineTraversal o => o l1 l2 h1 h2 -> AffineTraversal l1 l2 h1 h2
to_AffineTraversal o = AffineTraversal (match_and_replace o)
