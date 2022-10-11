module Fana.Optic.Concrete.Categories.Down
(
	Down (..), Down',
)
where

import Fana.Haskell.DescribingClass
import Fana.Haskell.Wrap (Wrap (..))
import Fana.Math.Algebra.Category.Functor.Pro
import Fana.Optic.Concrete.Categories.Interfaces
import Fana.Prelude

import qualified Fana.Haskell.Wrap as Wrap
import qualified Fana.Math.Algebra.Category.OnTypePairs as Cat2
import qualified Fana.Optic.Concrete.Common as Common


-- | A simple function at its core, can be used to go up along an optic.
newtype Down l1 l2 h1 h2 = Down (h1 -> l1)
type Down' l h = Common.Simple Down l h

instance Wrap.Wrap (Down l1 l2 h1 h2) where
	type Unwrap (Down l1 l2 h1 h2) = h1 -> l1

instance Cat2.Category Down where
	identity = Down id
	compose = Wrap.over_2 (<<<)

instance HasDescribingClass4 Down where
	type DescribingClass4Of Down = IsDown
	convert_from_describing_class_4 = down >>> Down


instance Profunctor (Down t1 t2) where
	dimap f1 _ (Down d) = Down (f1 >>> d)
	map_i f1 = Wrap.over (f1 >>>)
	map_o f2 (Down d) = Down d

instance IsFold Down where
	to_list (Down g) = g >>> (: [])
	map_fold (Down g) f = g >>> f
	fold (Down g) = g
	fold_r (Down g) combine r c = combine (g c) r
	fold_l (Down g) combine r c = combine r (g c)
instance IsDown Down where down = unwrap
