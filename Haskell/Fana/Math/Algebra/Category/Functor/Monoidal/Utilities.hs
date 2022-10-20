module Fana.Math.Algebra.Category.Functor.Monoidal.Utilities
(
	sequence_tuple_2,
)
where

import Control.Applicative (Applicative, liftA2)

sequence_tuple_2 :: Applicative a => (a x1, a x2) -> a (x1, x2)
sequence_tuple_2 (ax1, ax2) = liftA2 (,) ax1 ax2

