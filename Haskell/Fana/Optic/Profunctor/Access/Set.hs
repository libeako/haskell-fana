module Fana.Optic.Profunctor.Access.Set
(
	set, build,
)
where

import Fana.Math.Algebra.Category.Functor.Pro (Profunctor)
import Fana.Optic.Profunctor.Optic
import Fana.Prelude

import qualified Fana.Math.Algebra.Category.Functor.Pro as Profunctor
import qualified Fana.Optic.Concrete.Categories.Interfaces as Concrete
import qualified Fana.Optic.Concrete.Categories.Iso as Concrete


newtype Tagged a b = Tagged { deTagged :: b }

iso_Tagged :: Concrete.Iso b1 b2 (Tagged a1 b1) (Tagged a2 b2)
iso_Tagged = Concrete.Iso deTagged Tagged

instance Profunctor Tagged where
	dimap _ g = Concrete.fn_up iso_Tagged g

instance Profunctor.LoadableS Tagged where
	load_se = Concrete.fn_up iso_Tagged Right


set :: OpticBone (->) p1 p2 w1 w2 -> p2 -> (w1 -> w2)
set o p = o (const p)

build :: OpticBone Tagged p1 p2 w1 w2 -> p2 -> w2
build = Concrete.fn_down iso_Tagged
