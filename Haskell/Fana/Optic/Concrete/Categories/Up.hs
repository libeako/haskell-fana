module Fana.Optic.Concrete.Categories.Up
(
	Up (..), Up',
)
where

import Fana.Haskell.DescribingClass
import Fana.Math.Algebra.Category.Functor.Pro
import Fana.Optic.Concrete.Categories.Interfaces
import Fana.Prelude

import qualified Fana.Haskell.Wrap as Wrap
import qualified Fana.Math.Algebra.Category.OnTypePairs as Cat2
import qualified Fana.Optic.Concrete.Common as Common


-- | A simple function at its core, can be used to go up along an optic.
newtype Up l1 l2 h1 h2 = Up { unwrapUp :: l2 -> h2 }
type Up' l h = Common.Simple Up l h

instance Wrap.Wrap (Up l1 l2 h1 h2) where
	type Unwrap (Up l1 l2 h1 h2) = l2 -> h2

instance Cat2.Category Up where
	empty = Up id
	compose = Wrap.over_2 (>>>)

instance HasDescribingClass4 Up where
	type DescribingClass4Of Up = IsUp
	convert_from_describing_class_4 = up >>> Up


instance Profunctor (Up t1 t2) where
	dimap _ f2 (Up u) = Up (u >>> f2)
	map_i _ (Up u) = Up u
	map_o f2 (Up u) = Up (u >>> f2)

instance IsInterpret e Up where interpret = unwrapUp >>> map Right
instance IsUp Up where up = unwrapUp
