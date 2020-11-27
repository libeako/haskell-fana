module Fana.Data.List
(
	first, last,
	split_where,
	length_reaches,
)
where

import Fana.Prelude.FromBase
import Prelude ((<=), (-), Int)

import qualified Data.List as Base


continue :: r -> (e -> [e] -> r) -> [e] -> r
continue ce cht =
	\ case
		h : t -> cht h t
		[] -> ce

first :: [e] -> Maybe e
first = \case { [] -> Nothing; f : _ -> Just f }

last :: [e] -> Maybe e
last =
	\ case
		[] -> Nothing
		h : [] -> Just h
		h : t -> last t

{-| The splitting elements are not in the output. -}
split_where :: forall e . (e -> Bool) -> [e] -> [[e]]
split_where is_separator t =
	let
		step :: [e] -> Maybe ([e], [e])
		step list =
			if Base.null list
				then Nothing
				else
					let (head, tail) = Base.break is_separator list in
					if Base.null head
						then Nothing
						else Just (head, Base.dropWhile is_separator tail)
		in Base.reverse (Base.unfoldr step t)

length_reaches :: Int -> [x] -> Bool
length_reaches i l =
	if i <= 0
		then True
		else
			case l of
				[] -> False
				_ : t -> length_reaches (i-1) t
