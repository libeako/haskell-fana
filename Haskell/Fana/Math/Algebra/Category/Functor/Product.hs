module Fana.Math.Algebra.Category.Functor.Product
(
	get_1,
	get_2,
	iso_with_raw,
)
where

import Fana.Prelude.FromBase

import qualified Data.Functor.Product as Base
import qualified Fana.Optic.Concrete.Categories.Iso as Iso


get_1 :: Base.Product f g a -> f a
get_2 :: Base.Product f g a -> g a
get_1 (Base.Pair _1 __) = _1
get_2 (Base.Pair __ _2) = _2

-- | Isomorphism with raw pair.
iso_with_raw :: Iso.Iso (f x, g x) (f y, g y) (Base.Product f g x) (Base.Product f g y)
iso_with_raw = Iso.Iso (\ (Base.Pair _1 _2) -> (_1, _2)) (uncurry Base.Pair)


