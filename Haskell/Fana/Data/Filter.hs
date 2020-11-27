module Fana.Data.Filter
(
	Filterable (..),
)
where

import Fana.Prelude.FromBase

import qualified Control.Monad as Base
import qualified Data.Bool as Base
import qualified Data.Either as Base
import qualified Data.List as BaseList
import qualified Data.Map.Lazy as Base
import qualified Data.Map.Lazy as BaseMap
import qualified Data.Maybe as Base
import qualified Data.Maybe as BaseMaybe
import qualified Fana.Data.Either as Either


{-|
	Collections, whose elements can be filtered.

	@separate@ and @filter@ and the others are expressed in terms of each other.
	As you would expect. I am lazy here to unfold this relationship formally.

	Laws:

	* The [intuitive] relationship between separate and filter and the others.
	* Homomorphism

	Where by "homomorphism" i mean the following laws,
	similar to a kind of homomorphism between the monadic nature of Maybe and
	the compositionality of the filter function :

	>	let f x = map x >>> filter in
	>		f pure = id
	>		f join = filter >>> filter
-}
class Functor t => Filterable t where
	separate :: t (Either l r) -> (t l, t r)
	separate =
		liftA2 (,)
			(map (either Just (const Nothing)) >>> filter)
			(map (either (const Nothing) Just) >>> filter)
	filter :: t (Maybe e) -> t e
	filter = map Either.upgrade_Maybe >>> separate >>> snd
	map_filter :: (x -> Maybe y) -> t x -> t y
	map_filter = map >>> (>>> filter)
	keep :: forall e . (e -> Bool) -> t e -> t e
	keep =
		let
			per_element :: (e -> Bool) -> e -> Maybe e
			per_element p e = if p e then Just e else Nothing
			in per_element >>> map_filter
	delete :: forall e . (e -> Bool) -> t e -> t e
	delete = (>>> Base.not) >>> keep
	{-# MINIMAL separate | filter #-}


-- instances

instance Filterable Maybe where
	separate =
		\ case
			Nothing -> (Nothing, Nothing)
			Just ei ->
				case ei of
					Left l -> (Just l, Nothing)
					Right r -> (Nothing, Just r)
	filter = Base.join
	map_filter = maybe Nothing
	keep p = maybe Nothing (\ e -> if p e then Just e else Nothing)
	delete p = maybe Nothing (\ e -> if p e then Nothing else Just e)

instance Filterable [] where
	separate = Base.partitionEithers
	filter = Base.catMaybes
	map_filter = BaseMaybe.mapMaybe
	keep = BaseList.filter

instance Filterable (Base.Map k) where
	separate = Base.mapEither id
	filter = BaseMap.mapMaybe id
	map_filter = BaseMap.mapMaybe
	keep = BaseMap.filter
