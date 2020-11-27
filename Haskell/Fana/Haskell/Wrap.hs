-- | For easing the work with newtype wrappings.
module Fana.Haskell.Wrap
(
	Wrap (..),
	over, under, over_2, under_2,
)
where

import Data.Kind (Type)
import Fana.Prelude.FromBase
import Control.Applicative as App


class Wrap w where
	type Unwrap w :: Type
	wrap :: Unwrap w -> w
	unwrap :: w -> Unwrap w
	default wrap :: Coercible (Unwrap w) w => Unwrap w -> w
	default unwrap :: Coercible w (Unwrap w) => w -> Unwrap w
	wrap = coerce
	unwrap = coerce


over :: (Wrap x1, Wrap r) => (Unwrap x1 -> Unwrap r) -> (x1 -> r)
over f x1 = wrap (f (unwrap x1))

under :: (Wrap x1, Wrap r) => (x1 -> r) -> (Unwrap x1 -> Unwrap r)
under f x1 = unwrap (f (wrap x1))

over_2 :: (Wrap x1, Wrap x2, Wrap r) => (Unwrap x1 -> Unwrap x2 -> Unwrap r) -> (x1 -> x2 -> r)
over_2 f x1 x2 = wrap (f (unwrap x1) (unwrap x2))

under_2 :: (Wrap x1, Wrap x2, Wrap r) => (x1 -> x2 -> r) -> (Unwrap x1 -> Unwrap x2 -> Unwrap r)
under_2 f x1 x2 = unwrap (f (wrap x1) (wrap x2))


instance Wrap (App.ZipList e) where
	type Unwrap (App.ZipList e) = [e]
