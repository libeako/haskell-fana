{-|
	Suppose that type @t@ has a class that describes it totally.
	By 'describing totally' i mean that any value of any type that also has that class can be converted to @t@.
	This module provides a class for such types.
	More accuretely multiple classes,
	one for each n in 0..4 where n is the number of parameters in the kind of @t@.
-}
module Fana.Haskell.DescribingClass
(
	HasDescribingClass0 (..),
	HasDescribingClass1 (..),
	HasDescribingClass2 (..),
	HasDescribingClass3 (..),
	HasDescribingClass4 (..),
)
where

import Fana.Prelude.FromBase


class HasDescribingClass0 (t :: Type) where
	type DescribingClass0Of t :: Type -> Constraint
	convert_from_describing_class_0 :: DescribingClass0Of t t' => t'-> t

class HasDescribingClass1 (t :: Type -> Type) where
	type DescribingClass1Of t :: (Type -> Type) -> Constraint
	convert_from_describing_class_1 :: DescribingClass1Of t t' => t' p1 -> t p1

class HasDescribingClass2 (t :: Type -> Type -> Type) where
	type DescribingClass2Of t :: (Type -> Type -> Type) -> Constraint
	convert_from_describing_class_2 :: DescribingClass2Of t t' => t' p1 p2 -> t p1 p2

class HasDescribingClass3 (t :: Type -> Type -> Type -> Type) where
	type DescribingClass3Of t :: (Type -> Type -> Type -> Type) -> Constraint
	convert_from_describing_class_3 :: DescribingClass3Of t t' => t' p1 p2 p3 -> t p1 p2 p3

class HasDescribingClass4 (t :: Type -> Type -> Type -> Type -> Type) where
	type DescribingClass4Of t :: (Type -> Type -> Type -> Type -> Type) -> Constraint
	convert_from_describing_class_4 :: DescribingClass4Of t t' => t' p1 p2 p3 p4 -> t p1 p2 p3 p4
