module Fana.Data.Zippable
(
	Zippable (..),
)
where

import Fana.Prelude

import qualified Data.Map as Base
import qualified Fana.Data.HeteroPair as Pair
import qualified Prelude as Base


class Zippable t where
	zip ::
		Maybe (x -> r) -> Maybe (y -> r) -> (x -> y -> r) ->
		(t x -> t y -> t r)


instance Zippable Maybe where
	zip mfx mfy fxy x y =
		case (mfx, mfy, x, y) of
			(Just fx, _, Just ex, Nothing) -> Just (fx ex)
			(_, Just fy, Nothing, Just ey) -> Just (fy ey)
			(_, _, Just ex, Just ey) -> Just (fxy ex ey)
			_ -> Nothing

instance Zippable [] where
	zip mfx mfy fxy x y =
		case (mfx, mfy, x, y) of
			(Just fx, _, lx, []) -> map fx lx
			(_, Just fy, [], ly) -> map fy ly
			(_, _, hx : tx, hy : ty) -> fxy hx hy : zip mfx mfy fxy tx ty
			_ -> []

instance Base.Ord k => Zippable (Base.Map k) where
	zip mfx mfy fxy x y =
		let
			element_union (Just ex, Nothing) (Nothing, Just ey) = (Just ex, Just ey)
			element_union _ _ = (Nothing, Nothing)
			pairs_map =
				Base.unionWith element_union
					(map (Just >>> Pair.before Nothing) x)
					(map (Just >>> Pair.after Nothing) y)
			extract_from_pair =
				\ case
					(Just ex, Just ey) -> Just (fxy ex ey)
					(Just ex, Nothing) -> map ($ ex) mfx
					(Nothing, Just ey) -> map ($ ey) mfy
					(Nothing, Nothing) -> Nothing
			in Base.mapMaybe extract_from_pair pairs_map
