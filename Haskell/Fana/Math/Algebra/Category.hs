module Fana.Math.Algebra.Category
(
	before, after,
)
where

import qualified Control.Category as Base


before :: Base.Category c => c y z -> c x y -> c x z
after :: Base.Category c => c x y -> c y z -> c x z

before = (Base.<<<)
after = (Base.>>>)
