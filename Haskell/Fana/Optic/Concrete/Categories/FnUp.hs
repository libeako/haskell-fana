{-| Can move a function up. -}
module Fana.Optic.Concrete.Categories.FnUp
(
	FnUp (..), FnUp',
)
where

import Fana.Haskell.DescribingClass
import Fana.Math.Algebra.Category.Functor.Pro(Profunctor, dimap)
import Fana.Optic.Concrete.Categories.Interfaces
import Fana.Prelude

import qualified Fana.Haskell.Wrap as Wrap
import qualified Fana.Math.Algebra.Category.Functor.Pro as Profunctor
import qualified Fana.Math.Algebra.Category.OnTypePairs as Cat2
import qualified Fana.Optic.Concrete.Common as Common


newtype FnUp l1 l2 h1 h2 = FnUp { unwrapFnUp :: (l1 -> l2) -> (h1 -> h2) }
type FnUp' l h = Common.Simple FnUp l h


instance Wrap.Wrap (FnUp l1 l2 h1 h2) where
	type Unwrap (FnUp l1 l2 h1 h2) = (l1 -> l2) -> (h1 -> h2)


instance Cat2.Category FnUp where
	identity = FnUp id
	compose = Wrap.over_2 (>>>)

instance HasDescribingClass4 FnUp where
	type DescribingClass4Of FnUp = IsFnUp
	convert_from_describing_class_4 = fn_up >>> FnUp


instance IsFnUp FnUp where
	fn_up = unwrapFnUp
	-- | Fills all the focused elements with the given value.
	fill (FnUp c) = const >>> c


instance Profunctor (FnUp p1 p2) where dimap f1 f2 = Wrap.over (map (dimap f1 f2))
instance Profunctor.LoadableP (FnUp i1 i2) where load_pe = Wrap.over (map Profunctor.load_pe)
instance Profunctor.LoadableS (FnUp i1 i2) where load_se = Wrap.over (map Profunctor.load_se)
instance Profunctor.LoadablePS (FnUp i1 i2) where
