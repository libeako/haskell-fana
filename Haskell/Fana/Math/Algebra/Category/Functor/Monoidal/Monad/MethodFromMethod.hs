module Fana.Math.Algebra.Category.Functor.Monoidal.Monad.MethodFromMethod
(
	applicative_apply_from_monad_join,
	monad_apply_from_monad_join,
)
where

import Fana.Prelude.FromBase


applicative_apply_from_monad_join ::
	Functor f =>
	(forall t . f (f t) -> f t) -> (f (x -> y) -> f x -> f y)
applicative_apply_from_monad_join join f x = join (map (flip map x) f)

monad_apply_from_monad_join ::
	Functor f =>
	(forall t . f (f t) -> f t) -> (f x -> (x -> f y) -> f y)
monad_apply_from_monad_join join x f = join (map f x)
