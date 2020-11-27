module Fana.Math.Algebra.Category.OnTypePairs
(
	Category (..),
	(>**>), (<**<),
)
where

import Fana.Prelude.FromBase


-- | Category on pairs of types.
class Category c where
	empty :: c t1 t2 t1 t2
	compose :: c x1 x2 y1 y2 -> c y1 y2 z1 z2 -> c x1 x2 z1 z2

infixr 2 >**>
infixr 2 <**<

(>**>) :: Category c => c x1 x2 y1 y2 -> c y1 y2 z1 z2 -> c x1 x2 z1 z2
(>**>) = compose

(<**<) :: Category c => c y1 y2 z1 z2 -> c x1 x2 y1 y2 -> c x1 x2 z1 z2
(<**<) = flip (>**>)
