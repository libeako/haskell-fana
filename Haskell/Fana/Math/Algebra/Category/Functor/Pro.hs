module Fana.Math.Algebra.Category.Functor.Pro
(
	Profunctor, dimap, map_i, map_o,

	-- * Loadable profunctors
	LoadableP, load_pe, load_pl,
	LoadableS, load_se, load_sl,
	LoadablePS,

	-- * Else
	Over
)
where

import Fana.Haskell.Wrap (Wrap)
import Fana.Prelude

import qualified Data.Bifunctor as BiFr
import qualified Fana.Haskell.Wrap as Wrap
import qualified Fana.Data.HeteroPair as Pair
import qualified Fana.Data.Either as Either


class Profunctor p where

	dimap :: (x2 -> x1) -> (y1 -> y2) -> p x1 y1 -> p x2 y2
	-- | Map at the component of the profunctor which is the input in case of functions.
	map_i :: (x2 -> x1) -> p x1 y -> p x2 y
	-- | Map at the component of the profunctor which is the output in case of functions.
	map_o :: (y1 -> y2) -> p x y1 -> p x y2

	dimap f1 f2 = map_i f1 >>> map_o f2
	map_i f = dimap f id
	map_o f = dimap id f

	{-# MINIMAL dimap | map_i, map_o #-}


{-|
	Can load the profunctor to carry additional info in product structure.
	Known as "strong" in the Haskell community.

	- "p" means "product"
	- "e" means "early" position of the carried load in the product structure.
	- "l" means "late" position of the carried load in the product structure.
-}
class Profunctor p => LoadableP p where
	load_pe :: p a b -> p (c, a) (c, b)
	load_pl :: p a b -> p (a, c) (b, c)
	load_pe = load_pl >>> dimap Pair.swap Pair.swap
	load_pl = load_pe >>> dimap Pair.swap Pair.swap
	{-# MINIMAL load_pe | load_pl #-}


{-|
	Can load the profunctor to carry additional info in sum structure.
	Known as "choice" in the Haskell community.

	- "s" means "sum"
	- "e" means "early" position of the carried load in the sum structure.
	- "l" means "late" position of the carried load in the sum structure.
-}
class Profunctor p => LoadableS p where
	load_se :: p a b -> p (Either c a) (Either c b)
	load_sl :: p a b -> p (Either a c) (Either b c)
	load_se = load_sl >>> dimap Either.swap Either.swap
	load_sl = load_se >>> dimap Either.swap Either.swap
	{-# MINIMAL load_se | load_sl #-}


class (LoadableP p, LoadableS p) => LoadablePS p where


instance Profunctor (->) where
	dimap fx fy f = fx >>> f >>> fy
	map_i fx f = fx >>> f
	map_o fy f = f >>> fy

instance LoadableP (->) where
	-- load_pe :: p a b -> p (c, a) (c, b)
	-- load_pl :: p a b -> p (a, c) (b, c)
	load_pe = BiFr.second
	load_pl = BiFr.first

instance LoadableS (->) where
	-- load_se :: p a b -> p (Either c a) (Either c b)
	-- load_sl :: p a b -> p (Either a c) (Either b c)
	load_se= BiFr.second
	load_sl = BiFr.first

instance LoadablePS (->) where


-- over some functors

newtype Over fx fy p x y = Over (p (fx x) (fy y))

instance Wrap (Over fx fy p x y) where
	type Unwrap (Over fx fy p x y) = p (fx x) (fy y)

instance (Profunctor p, Functor fx, Functor fy) => Profunctor (Over fx fy p) where
	dimap tx ty = Wrap.over (dimap (map tx) (map ty))
