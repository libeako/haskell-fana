module Fana.Math.Algebra.Category.Functor.Monoidal.Monad.FreeOverFunctor
(
	Mo (..),
	Om (..),
)
where

{-| Monad, free over functor. -}
data Mo f e = Join (f (Mo f e)) | Pure e

{-| Co-Monad, co-free over functor. -}
data Om f e = Om
	{
		duplicate :: f (Om f e),
		extract :: e
	}

