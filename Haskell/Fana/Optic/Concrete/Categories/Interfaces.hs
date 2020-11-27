module Fana.Optic.Concrete.Categories.Interfaces
(
	IsFold (..), IsInterpret (..),
	IsDown (..), IsUp (..),
	IsFnUp (..), IsTraversal (..), IsAffineTraversal (..),
	IsLens (..), IsPrism (..),
	IsPartialIso (..),
	IsIso (..),
)

where

import Fana.Math.Algebra.Category.Functor.Pro
import Fana.Prelude
import Prelude (fst, snd, Monoid)

import qualified Data.Foldable as Foldable


class (forall t1 t2 . Profunctor (o t1 t2)) => IsFold o where

	to_list :: o l1 l2 h1 h2 -> h1 -> [l1]
	to_list o = fold_r o (:) []

	map_fold :: Monoid r => o l1 l2 h1 h2 -> (l1 -> r) -> h1 -> r
	map_fold o tr = to_list o >>> Foldable.foldMap tr

	fold :: Monoid m => o m x h1 h2 -> h1 -> m
	fold o = map_fold o id

	fold_l :: o l1 l2 h1 h2 -> (r -> l1 -> r) -> r -> h1 -> r
	fold_l o combine r = to_list o >>> Foldable.foldl' combine r

	fold_r :: o l1 l2 h1 h2 -> (l1 -> r -> r) -> r -> h1 -> r
	fold_r o combine r = to_list o >>> Foldable.foldr combine r

	{-# MINIMAL to_list | fold_r #-}

class (forall t1 t2 . Profunctor (o t1 t2)) => IsInterpret e o where
	interpret :: o l1 l2 h1 h2 -> l2 -> Either e h2

class IsFold o => IsDown o where down :: o l1 l2 h1 h2 -> h1 -> l1
class (forall e . IsInterpret e o) => IsUp o where
	up :: o l1 l2 h1 h2 -> l2 -> h2

class (forall t1 t2 . Profunctor (o t1 t2)) => IsFnUp o where
	fn_up :: o e1 e2 c1 c2 -> (e1 -> e2) -> (c1 -> c2)
	fill :: o e1 e2 c1 c2 -> e2 -> c1 -> c2
	fill o = const >>> fn_up o
	{-# MINIMAL fn_up #-}

class (IsFold o, IsFnUp o) => IsTraversal o where
	traverse :: o l1 l2 h1 h2 -> forall a . Applicative a => (l1 -> a l2) -> (h1 -> a h2)

{-|
	Prefer to implement "match_and_replace"
	because that way is generally more run-time efficient.
-}
class IsTraversal o => IsAffineTraversal o where
	match_and_replace :: o l1 l2 h1 h2 -> h1 -> (Either h2 l1, l2 -> h2)
	match_and_replace o h1 = (match o h1, replace o h1)
	match :: o l1 l2 h1 h2 -> h1 -> Either h2 l1
	match = match_and_replace >>> map fst
	replace :: o l1 l2 h1 h2 -> h1 -> l2 -> h2
	replace = match_and_replace >>> map snd
	{-# MINIMAL match_and_replace | match, replace #-}


class (IsDown o, IsAffineTraversal o) => IsLens o where
	down_and_replace :: o l1 l2 h1 h2 -> h1 -> (l1, l2 -> h2)
	down_and_replace o = liftA2 (,) (down o) (replace o)
	{-# MINIMAL #-}

class (IsUp o, IsAffineTraversal o) => IsPrism o where

class (IsDown o, IsInterpret e o) => IsPartialIso e o where

class (forall e . IsPartialIso e o, IsDown o, IsUp o) => IsIso o where
