module Fana.Data.HomoPair
(
	Naked,
	map_homo_pair,
	HomoPair (..),
	make,
	elem_1, elem_2,
	AllKeys, all_keys,
	curry, uncurry,
	transpose_to_naked,
	Combiner,
)
where

import Control.Arrow ((&&&))
import Fana.Prelude hiding (curry, uncurry)

import qualified Data.Distributive as Functor
import qualified Data.Functor.Rep as Functor
import qualified Fana.Haskell.Wrap as Wrap
import qualified Fana.Data.Function as Fn
import qualified Data.Tuple as Tuple
import qualified Prelude as Base


type Naked e = (e, e)


map_homo_pair :: (x -> y) -> Naked x -> (y, y)
map_homo_pair f x = (f (fst x), f (snd x))


newtype HomoPair e = HomoPair { unwrapHomoPair :: Naked e}
	deriving (Functor, Foldable, Traversable)

instance Wrap.Wrap (HomoPair e) where
	type Unwrap (HomoPair e) = Naked e

make :: e -> e -> HomoPair e
make e1 e2 = HomoPair (e1, e2)

type GetElem e = Fn.DownFrom HomoPair e
type GetElemForall = Fn.DownFromForall HomoPair

elem_1, elem_2 :: GetElem e
elem_1 = unwrapHomoPair >>> Tuple.fst
elem_2 = unwrapHomoPair >>> Tuple.snd

type AllKeys = HomoPair GetElemForall

all_keys :: AllKeys
all_keys =
	HomoPair
		(
			(Fn.DownFromForall elem_1),
			(Fn.DownFromForall elem_2)
		)


curry :: (HomoPair e -> r) -> (e -> e -> r)
curry f = Base.curry (HomoPair >>> f)

uncurry :: (e -> e -> r) -> (HomoPair e -> r)
uncurry f c = f (elem_1 c) (elem_2 c)


transpose_to_naked :: HomoPair (x, y) -> (HomoPair x, HomoPair y)
transpose_to_naked = map fst &&& map snd


instance Functor.Distributive HomoPair where
	distribute in_functor =
		map (Fn.unwrapDownFromForAll >>> flip map in_functor) all_keys

instance Functor.Representable HomoPair where
	type Rep HomoPair = GetElemForall
	tabulate = flip map all_keys
	index = flip Fn.unwrapDownFromForAll

{-
	TODO :
		concretize this implementation for faster execution
-}
instance Base.Applicative HomoPair where
	pure x = make x x
	f <*> x =
		let
			per_component component =
				let
					c = component
					o = Fn.unwrapDownFromForAll
					in (o c f) (o c x)
			in map per_component all_keys


type Combiner x = HomoPair x -> x
