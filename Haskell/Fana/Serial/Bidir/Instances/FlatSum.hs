{-|
	"Flat" sum is a direct sum of list of types,
	as one that is not built from binary sums.
-}
module Fana.Serial.Bidir.Instances.FlatSum
(
	FlatSumCase (..), flat_sum,
)
where

import Fana.Data.Tree.Leaf (Tree)
import Fana.Prelude.FromBase
import Fana.Serial.Bidir.Parse (Parser)
import Fana.Serial.Bidir.Serializer

import qualified Prelude as Base
import qualified Data.Foldable as Fold
import qualified Data.List as List
import qualified Data.Maybe as Base
import qualified Control.Monad.Except as Monad
import qualified Fana.Math.Algebra.Monoid.Accumulate as Accu
import qualified Fana.Optic.Concrete.Prelude as Optic
import qualified Fana.Serial.Bidir.Parse as Parse
import qualified Fana.Serial.Bidir.Instances.Maybe as Mb
import qualified Fana.Serial.Bidir.Instances.WithStartPosition as Wsp


type Text = Parse.ErrorText

{-|
	One case of a flat sum.

	* s is the [conceptual] sum type.

	Contains 2 serialization layers.
	The lower one works on the stream and connects it to an intermediate value level,
	which technically is the type of the value that this sum case serializes.
	The higher one connects the intermediate to the sum type level.
	I use the name @m@ for the type of the intermediate level.
-}
data FlatSumCase c c' s s' =
	forall m m' .
	FlatSumCase
	{
		fsc_description :: Text,
		fsc_serializer_low :: Serializer c c' m m',
		fsc_serializer_high :: Optic.PartialIso () s' s m' m,
		-- | Whether the parser sends error upward in the serializer hierarchy.
		fsc_propagates_error :: Bool
	}

{-|
	Renders the given sum typed value
	with the given case if they match.
	Otherwise the output is empty.
-}
maybe_render_flat_sum :: FlatSumCase c c' s s' -> s -> [Tree [] c]
maybe_render_flat_sum =
	\ case
		FlatSumCase _ ser_l ser_h _ ->
			Optic.piso_interpret ser_h >>> map (renderer ser_l) >>> Fold.concat

{-|
	Returns the first nothing value - iff one exists.
-}
search_existant :: [Maybe m] -> Maybe m
search_existant = Base.catMaybes >>> Base.listToMaybe

{-|
	Creates a renderer for a sum type that will use the given cases.
-}
render_flat_sum :: [FlatSumCase c c' s s'] -> Renderer c s
render_flat_sum cs r = Fold.concat (map (flip maybe_render_flat_sum r) cs)

{-|
	Creates a parser for a sum type that will use the given case.
-}
parse_flat_sum_case :: forall c c' s s' . FlatSumCase c c' s s' -> Parser c' (Maybe s')
parse_flat_sum_case (FlatSumCase _ ser_l ser_h propagates_error) =
	(if propagates_error then map Just else Mb.parse_if_can) (map (Optic.piso_down ser_h) (parser ser_l))

{-|
	Composes the 2 given parser into 1
	that uses either successful one of the input ones.
	The first one is preferred.
-}
choose_of_2_parsers :: Parser p (Maybe v) -> Parser p (Maybe v) -> Parser p (Maybe v)
choose_of_2_parsers m y =
	do
		vx <- m
		if Base.isJust vx then pure vx else y

{-|
	Composes the given parsers into 1
	that uses either successful one of the input ones.
	The earlier ones are preferred.
-}
choose_of_many_parsers :: [FlatSumCase c c' s s'] -> Parser c' (Maybe s')
choose_of_many_parsers = map parse_flat_sum_case >>> Fold.foldr choose_of_2_parsers (pure Nothing)

parse_flat_sum :: forall c c' s s' . [FlatSumCase c c' s s'] -> Parser c' s'
parse_flat_sum cases =
	let
		error_desc :: Accu.Accumulated Text
		error_desc =
			let case_list = List.intercalate ", " (map fsc_description cases)
			in Base.foldMap Accu.single ["all possible cases [", case_list, "] failed"]
		extract :: Maybe c' -> Maybe s' -> Parser c' s'
		extract start_pos =
			let err = Monad.throwError [Parse.Error start_pos error_desc]
			in Base.maybe err pure
		parse' :: Maybe c' -> Parser c' s'
		parse' start_pos = choose_of_many_parsers cases >>= extract start_pos
		in Wsp.with_start_position parse'

{-|
	Serializer of a flat sum.
-}
flat_sum :: [FlatSumCase c c' s s'] -> Serializer c c' s s'
flat_sum = liftA2 Serializer render_flat_sum parse_flat_sum
