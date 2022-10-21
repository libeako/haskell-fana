module Fana.Data.Identified
(
	Identified (..),
)
where

import Prelude (Eq (..), Ord (..), Functor, Foldable, Traversable)

{-|
	Suff that is identified uniquely up to equality.
	Value of this type are members of a set theoretical function from id to cargo.
-}
data Identified i o =
	Identified { id :: i, cargo :: o }
	deriving (Functor, Foldable, Traversable)

instance Eq i => Eq (Identified i o) where
	Identified o1 _ == Identified o2 _ = (o1 == o2)

instance Ord i => Ord (Identified i o) where
	compare (Identified i1 _) (Identified i2 _) = compare i1 i2
