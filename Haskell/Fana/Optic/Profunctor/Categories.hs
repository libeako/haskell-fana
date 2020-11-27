module Fana.Optic.Profunctor.Categories
(
	Iso,
	Iso',
	reverse_iso,

	Lens,
	Lens',

	Prism,
	Prism',

	AffineTraversal,
	AffineTraversal',

	Traversal,
	Traversal',
)
where

import Fana.Math.Algebra.Category.Functor.Pro(Profunctor)
import Fana.Optic.Profunctor.Optic

import qualified Fana.Math.Algebra.Category.Functor.Pro as Profunctor
import qualified Fana.Optic.Concrete.Categories.Iso as Concrete
import qualified Fana.Optic.Laarhoven.Traverse as OpticF
import qualified Fana.Optic.Profunctor.Interface as If



-- | Isomorphism
type Iso p1 p2 w1 w2 = forall tf . Profunctor tf => OpticBone tf p1 p2 w1 w2
-- | Simple Isomorphism
type Iso' p w = TypeKeeper Iso p w

reverse_iso :: Iso p1 p2 w1 w2 -> Iso w2 w1 p2 p1
reverse_iso iso = If.from_adapted (Concrete.reverse (If.to_adapted iso))


type Lens p1 p2 w1 w2 = OpticNude Profunctor.LoadableP p1 p2 w1 w2
type Lens' p w = TypeKeeper Lens p w


type Prism p1 p2 w1 w2 = OpticNude Profunctor.LoadableS p1 p2 w1 w2
type Prism' p w = TypeKeeper Prism p w


type AffineTraversal p1 p2 w1 w2 =
	forall tf . (Profunctor.LoadableP tf, Profunctor.LoadableS tf) => OpticBone tf p1 p2 w1 w2
type AffineTraversal' p w = TypeKeeper AffineTraversal p w


type Traversal p1 p2 w1 w2 = forall tf . OpticF.Wandering tf => OpticBone tf p1 p2 w1 w2
type Traversal' p w = TypeKeeper Traversal p w
