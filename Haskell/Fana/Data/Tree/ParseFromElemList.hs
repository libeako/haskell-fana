{-|
	Parsing a tree from the list of its nodes given as (hight * value) pairs.
-}
module Fana.Data.Tree.ParseFromElemList
(
	Hight,
	HightListParseError (..),
	parse, serializer,
)
where

import Control.Monad (return)
import Data.Tree (Tree (..))
import Fana.Data.Tree.OfBase (with_hight)
import Fana.Math.Algebra.Monoid.Accumulate
import Fana.Prelude
import Prelude ((+), (<), (>), String)

import qualified Control.Monad.State.Strict as Mtl
import qualified Control.Monad.Except as Mtl
import qualified Data.Bifunctor as Bifunctor
import qualified Data.Foldable as Foldable
import qualified Fana.Data.Tree.OfBase as OfBase
import qualified Fana.Optic.Concrete.Prelude as Optic
import qualified Fana.Serial.Print.Show as Show


type Hight = OfBase.Hight

show_data_with_hight :: Show.Showable String e => (Hight, e) -> Accumulated String
show_data_with_hight (hight, d) = "with hight " <> Show.from_Show hight <> " : " <> Show.show d

newtype HightListParseError e = HightListParseError
	{
	-- | the first element in the input list that is erroneous
	-- [has n more hight than its parent predecessor, where n > 1].
	hilipeFirstWrong :: (Hight, e)
	}

instance Show.Showable String e => Show.Showable String (HightListParseError e) where
	show (HightListParseError elem) = "error parsing hight list at (" <> show_data_with_hight elem <> ")"

-- | . the error info contains the first invalid element of the input list;
type HightListParserMonadStack e = Mtl.StateT [(Hight, e)] (Either (HightListParseError e))

type HightListParserIntoForest e = HightListParserMonadStack e [Tree e]
type HightListParserIntoTree e = HightListParserMonadStack e (Maybe (Tree e))

parser_of_forest_at_hight :: Hight -> HightListParserIntoForest e
parser_of_forest_at_hight hight =
	do
		mbx <- parser_of_tree_at_hight hight
		case mbx of
			Nothing -> return []
			Just x -> map (x :) (parser_of_forest_at_hight hight)

parser_of_tree_at_hight :: forall e . Hight -> HightListParserIntoTree e
parser_of_tree_at_hight hight =
	do
		s <- Mtl.get
		case s of
			[] -> return Nothing
			next_line : rest ->
				let
					(next_line_hight, next_line_content) = next_line
					proceed_normally :: HightListParserMonadStack e (Tree e)
					proceed_normally =
						do
							_ <- Mtl.modify (const rest)
							children <- parser_of_forest_at_hight (hight + 1)
							return (Node next_line_content children)
					tree :: HightListParserMonadStack e (Tree e)
					tree =
						if next_line_hight > hight + 1 then Mtl.throwError (HightListParseError next_line)
							else proceed_normally
					in
						if next_line_hight < hight then return Nothing
							else
								if next_line_hight == hight then map Just proceed_normally
									else Mtl.throwError (HightListParseError next_line)

parse :: [(Hight, e)] -> Either (HightListParseError e) [Tree e]
parse = Mtl.evalStateT (parser_of_forest_at_hight 0)


serializer :: Optic.PartialIso' (HightListParseError e) [(Hight, e)] [Tree e]
serializer =
	let
		render =
			map (with_hight >>> map (Bifunctor.first (+ (- 1)) >>> uncurry (,)) >>> Foldable.toList)
			>>> Foldable.concat
		in Optic.PartialIso render parse
